{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM_, void, when)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson (Result(..), fromJSON)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Default (def)
import qualified Data.HashMap.Strict as M
import           Data.IORef
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics (Generic)
import           GHC.IO.FD (openFile)
import           Network.HTTP.Conduit (simpleHttp)
import           Network.TLS ( Params(pConnectVersion, pAllowedVersions, pCiphers)
                             , Version(TLS10, TLS11, TLS12)
                             , defaultParamsClient )
import           Network.TLS.Extra (ciphersuite_medium)
import           Network.Wai.Logger (clockDateCacher)
import           Network.Xmpp
import           Network.Xmpp.IM (simpleIM)
import           System.Environment (getArgs)
import           System.IO (IOMode(AppendMode))
import           System.Log.FastLogger
import           System.Timeout (timeout)
import           Text.Feed.Import (parseFeedString)
import qualified Text.Feed.Types as F
import           Text.Feed.Query ( feedItems, getItemTitle
                                 , getItemPublishDate, getItemLink)
import qualified Web.Twitter as TT
import           Web.Twitter.OAuth (Consumer(..), singleAccessToken)

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

data Bot = Bot
    { xmppUsername       :: Text
    , xmppPassword       :: Text
    } deriving (Show, Generic)
instance FromJSON Bot

data Twitter = Twitter
    { consumerKey       :: String
    , consumerSecret    :: String
    , oauthToken        :: String
    , oauthTokenSecret  :: String
    } deriving (Show, Generic)
instance FromJSON Twitter

data Feeds = Feeds
    { name :: String
    , url  :: String
    } deriving (Show, Generic)
instance FromJSON Feeds

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FeedItem
    source String
    title String
    date  String
    link  String
    UniqueItem title date
    deriving Show
|]

mkConnStr :: Database -> IO C.ByteString
mkConnStr s = return $ C.pack $ "host=" ++ host s ++
                                " dbname=" ++ database s ++
                                " user=" ++ user s ++
                                " password=" ++ password s ++
                                " port=" ++ show (port s)

getFeed :: Feeds -> IO [FeedItem]
getFeed feedsource = do
    result <- simpleHttp $ url feedsource
    let feed = parseFeedString $ LC.unpack result
    let items = case feed of
                    Just f  -> fromMaybe [] $
                               mapM (makeItem $ name feedsource) (feedItems f)
                    Nothing -> []
    return items

-- | Make an item from a feed item.
makeItem :: String -> F.Item -> Maybe FeedItem
makeItem src item =
    FeedItem <$> Just src
             <*> getItemTitle item
             <*> getItemPublishDate item
             <*> getItemLink item

pprFeed :: FeedItem -> Text
pprFeed item = T.pack $
    "[" ++ feedItemSource item ++ "]: "
        ++ feedItemTitle item ++ " " ++ feedItemLink item

getTwitter :: Twitter -> IO [FeedItem]
getTwitter s = do
    let consumer = Consumer (consumerKey s) (consumerSecret s)
    tok <- singleAccessToken consumer (oauthToken s) (oauthTokenSecret s)
    st <- TT.homeTimeline tok []
    return $ map makeStatus st
  where
    makeStatus st = FeedItem ('@' : TT.user st) (TT.text st) (TT.id_str st) ""

loadYaml :: String -> IO (M.HashMap Text Value)
loadYaml fp = do
    mval <- decodeFile fp
    case mval of
        Nothing  -> error $ "Invalid YAML file: " ++ show fp
        Just obj -> return obj

parseYaml :: FromJSON a => Text -> M.HashMap Text Value -> a
parseYaml k hm =
    case M.lookup k hm of
        Just val -> case fromJSON val of
                        Success s -> s
                        Error err -> error $ "Falied to parse "
                                           ++ T.unpack k ++ ": " ++ show err
        Nothing  -> error $ "Failed to load " ++ T.unpack k
                                              ++ " from config file"

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
    config <- loadYaml "config/bot.yml"
    let bot = parseYaml "Bot" config :: Bot

    args <- getArgs
    case args of
        [] -> loop bot
        xs -> mapM_ (handleFeed config) xs

loop :: Bot -> IO ()
loop bot = return ()

handleFeed :: M.HashMap Text Value -> String -> IO ()
handleFeed config list = do
    (fd, _) <- openFile "bot.log" AppendMode True
    logger <- newLoggerSet defaultBufSize fd
    let logI = writeLog logger
    let db = parseYaml "Database" config :: Database
        bot = parseYaml "Bot" config :: Bot
        twitter = parseYaml "Twitter" config :: Twitter
    feedList <- loadYaml list
    let feeds = parseYaml "Feeds" feedList :: [Feeds]
        contact = parseYaml "Contact" config :: String
        contactJid = parseJid contact
    connStr <- mkConnStr db
    result <-
        session "google.com"
                (Just (const [plain (xmppUsername bot)
                                    Nothing
                                    (xmppPassword bot)], Nothing))
                def { sessionStreamConfiguration = def
                        { tlsParams = defaultParamsClient
                            { pConnectVersion = TLS10
                            , pAllowedVersions = [TLS10, TLS11, TLS12]
                            , pCiphers = ciphersuite_medium } } }
    sess <- case result of
                Right s -> logI  "Session created." >> return s
                Left e -> logI "Session Failed." >>
                          error ("XmppFailure: " ++ show e)
    sendPresence def sess

    -- If contact is offline, terminate!
    pres <- timeout 10000000 $ waitForPresence
        (\p -> fmap toBare (presenceFrom p) == Just contactJid) sess

    when (fmap presenceType pres == Just Available) $ do
        withPostgresqlPool connStr (poolsize db) $ \pool ->
            flip runSqlPersistMPool pool $ runMigration migrateAll

        -- Insert feeds to DB if not exist and send to contact
        forM_ feeds $ \f -> do
            items <- getFeed f
            msg <- newIORef [] :: IO (IORef [FeedItem])
            withPostgresqlPool connStr (poolsize db) $ \pool ->
                flip runSqlPersistMPool pool $ do
                    forM_ items $ \j -> do
                        ent <- insertBy j
                        case ent of
                            Left (Entity _ _) -> liftIO $ return ()
                            Right _ -> liftIO $ modifyIORef msg (j:)
                    liftIO $ do
                        toSend <- readIORef msg
                        logI (name f ++ " fetched: " ++ show (length toSend))
                        let longMsg = T.unlines $ map pprFeed toSend
                            payload = length $ T.unpack longMsg
                        when (payload /= 0) $ do
                            let header = simpleIM contactJid (T.pack $ name f ++ ": " ++ show payload)
                            void $ sendMessage header sess
                            let reply = simpleIM contactJid longMsg
                            void $ sendMessage reply sess
                        logI (name f ++ " sended.")

        -- Insert twitter status to DB if not exist
        status <- getTwitter twitter
        logI ("Twitter timeline fetched: " ++ show (length status))
        withPostgresqlPool connStr (poolsize db) $ \pool ->
            flip runSqlPersistMPool pool $
                forM_ status $ \j -> do
                    ent <- insertBy j
                    case ent of
                        Left (Entity _ _) -> liftIO $ return ()
                        Right _ -> liftIO $ do
                            expanded <- expandShortUrl (feedItemTitle j)
                            let reply = simpleIM contactJid (pprFeed j {feedItemTitle = expanded})
                            void $ sendMessage reply sess
        logI "Twitter timeline sended."
    rmLoggerSet logger
    sendPresence presenceOffline sess
    endSession sess
