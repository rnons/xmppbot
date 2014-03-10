{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.Reader
import           Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as C
import           Data.Either (rights)
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml.Config
import           Database.Persist
import           Database.Persist.Postgresql
import           GHC.Generics (Generic)
import           GHC.IO.FD (openFile)
import           Network.HTTP.Conduit (Manager, newManager, closeManager)
import           Network.HTTP.Client (defaultManagerSettings)
import           Network.Wai.Logger (clockDateCacher)
import           Network.Xmpp
import           Network.Xmpp.IM
import           Network.Xmpp.Lens (set)
import           Prelude hiding (lookup)
import           System.Environment (getArgs)
import           System.IO (IOMode(AppendMode))
import           System.Log.FastLogger
import           System.Timeout (timeout)
import           Web.Twitter.OAuth (Consumer(..), singleAccessToken)

import Model
import Xmppbot.Feed
import Xmppbot.Greeting
import Xmppbot.Translate (translate)
import Xmppbot.Twitter (expandShortUrl)

data Database = Database
    { user      :: String
    , password  :: String
    , host      :: String
    , port      :: Int
    , database  :: String
    , poolsize  :: Int
    } deriving (Show, Generic)
instance FromJSON Database

data Xmpp = Xmpp
    { xmppUsername       :: Text
    , xmppPassword       :: Text
    } deriving (Show, Generic)
instance FromJSON Xmpp

data Bot = Bot
    { botSession :: Session
    , botManager :: Manager
    }

type Xmppbot = ReaderT Bot IO

xmppConf :: SessionConfiguration
xmppConf = set tlsServerIdentificationL ("talk.google.com", "") def

runXmppbot :: Session -> Manager -> Xmppbot () -> IO ()
runXmppbot sess manager act = runReaderT act (Bot sess manager)

mkConnStr :: Database -> C.ByteString
mkConnStr s = C.pack $ "host=" ++ host s ++
                       " dbname=" ++ database s ++
                       " user=" ++ user s ++
                       " password=" ++ password s ++
                       " port=" ++ show (port s)

writeLog :: LoggerSet -> String -> IO ()
writeLog logger msg = do
    (loggerDate, _) <- clockDateCacher
    now <- loggerDate
    pushLogStr logger $
        toLogStr now <> " " <> toLogStr msg <> "\n"

-- Xmppbot can be run in two modes:
-- Interactive mode:
--     When no args provided, run forever.
-- Feed mode:
--     Provide the name of config files, which contain contact and feed list.
--     Fetch and send feeds, then exit.
main :: IO ()
main = do
    config <- load "config/bot.yml"

    args <- getArgs
    sess <- newSession config
    sendPresence def sess
    case args of
        [] -> do
            manager <- newManager defaultManagerSettings
            runXmppbot sess manager loop
            closeManager manager
        xs -> mapM_ (handleFeed config sess) xs

    sendPresence presenceOffline sess
    endSession sess

newSession :: Config -> IO Session
newSession config = do
    bot <- lookup "Bot" config
    result <-
        session "google.com"
                (simpleAuth (xmppUsername bot) (xmppPassword bot))
                xmppConf
    case result of
        Right s -> putStrLn "Session created." >> return s
        Left e  -> putStrLn "Session Failed." >>
                   error ("XmppFailure: " ++ show e)

loop :: Xmppbot ()
loop = do
    sess <- botSession `liftM` ask
    manager <- botManager `liftM` ask
    lift $ forever $ do
        msg <- getMessage sess
        tr <- forM (imBody $ fromJust $ getIM msg) $ \m ->
            translate manager $ T.unpack $ bodyContent m
        let body = map (MessageBody Nothing . T.pack) tr
        case answerIM body msg of
            Just answer -> void $ sendMessage answer sess
            Nothing -> putStrLn "Received message with no sender."

handleFeed :: Config -> Session -> String -> IO ()
handleFeed config sess list = do
    (fd, _) <- openFile "bot.log" AppendMode True
    logger <- newStdoutLoggerSet defaultBufSize
    db <- lookup "Database" config
    tk <- lookup "Twitter" config
    feedList <- load list
    contact <- lookup "Contact" feedList
    let feeds = lookupDefault "Feeds" [] feedList
        twitterUsers = lookupDefault "TwitterUser" [] feedList
        greetings = lookupDefault "Greetings" [] feedList
        homeTL = lookupDefault "HomeTL" False feedList
        contactJid = parseJid contact

    -- If contact is offline, terminate!
    pres <- timeout 10000000 $ waitForPresence
        (\p -> fmap toBare (presenceFrom p) == Just contactJid) sess

    unless (null greetings) $ do
        msg <- pick greetings
        let msgC = simpleIM contactJid msg
        void $ sendMessage msgC sess

    when (fmap presenceType pres == Just Available) $ do
        pool <- createPostgresqlPool (mkConnStr db) (poolsize db)
        flip runSqlPersistMPool pool $ runMigration migrateAll

        -- Insert feeds to DB if not exist and send to contact
        forM_ feeds $ \f -> do
            entries <- getFeed contact f
            send entries pool sess contact False

        -- Insert twitter status to DB if not exist
        let consumer = Consumer (consumerKey tk) (consumerSecret tk)
        tok <- singleAccessToken consumer (oauthToken tk) (oauthTokenSecret tk)
        forM_ twitterUsers $ \u -> do
            entries <- getUserTL tok u contact
            send entries pool sess contact True
        when homeTL $ do
            tweets <- getHomeTL tok contact
            send tweets pool sess contact True

    rmLoggerSet logger

send :: [FeedItem] -> ConnectionPool -> Session -> String -> Bool -> IO ()
send entries pool sess contact isTwitter = do
    let contactJid = parseJid contact
    toSend <- fmap rights $ forM entries $ \e ->
        flip runSqlPersistMPool pool $ do
            ent <- insertBy e
            case ent of
                Left (Entity _ _) -> return $ Left False
                Right _ -> return $ Right e
    longMsg <- if isTwitter
                   then do
                       msg <- forM toSend $ \s -> do
                           expanded <- expandShortUrl (feedItemTitle s)
                           return $ s { feedItemTitle = expanded }
                       return $ T.unlines $ map pprFeed msg
                   else return $ T.unlines $ map pprFeed toSend
    let payload = length $ T.unpack longMsg
    when (payload /= 0) $ do
        let reply = simpleIM contactJid longMsg
        void $ sendMessage reply sess
