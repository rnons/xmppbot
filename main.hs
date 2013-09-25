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
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Network.TLS ( Params(pConnectVersion, pAllowedVersions, pCiphers)
                             , Version(TLS10, TLS11, TLS12)
                             , defaultParamsClient )
import           Network.TLS.Extra (ciphersuite_medium)
import           Network.Xmpp
import           Network.Xmpp.IM (simpleIM)
import           System.Exit (ExitCode(..))
import           System.IO (openFile, IOMode(..))
import           System.Log.FastLogger
import           System.Posix.Process (exitImmediately)
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

data Account = Account
    { botUsername       :: Text
    , botPassword       :: Text
    , contactUsername   :: Text
    } deriving (Show, Generic)

data Twitter = Twitter
    { consumerKey :: String
    , consumerSecret    :: String
    , oauthToken        :: String
    , oauthTokenSecret  :: String
    } deriving (Show, Generic)

data Feeds = Feeds
    { name :: String
    , url  :: String
    } deriving (Show, Generic)

instance FromJSON Database
instance FromJSON Account
instance FromJSON Twitter
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
parseYaml key hm =
    case M.lookup key hm of
        Just val -> case fromJSON val of
                        Success s -> s
                        Error err -> error $ "Falied to parse " 
                                           ++ T.unpack key ++ ": " ++ show err
        Nothing  -> error $ "Failed to load " ++ T.unpack key 
                                              ++ " from config file"

writeLog :: Logger -> String -> IO ()
writeLog logger msg = do
    date <- loggerDate logger
    loggerPutStr logger
        [ LB date
        , LB " "
        , LS msg
        , LB "\n"
        ]
        
main :: IO ()
main = do
    logger <- mkLogger True =<< openFile "bot.log" AppendMode
    let log = writeLog logger
    config <- loadYaml "bot.yml"
    let db = parseYaml "Database" config :: Database
        account = parseYaml "Account" config :: Account
        twitter = parseYaml "Twitter" config :: Twitter
        feeds = parseYaml "Feeds" config :: [Feeds]
        contactJid = parseJid $ T.unpack $ contactUsername account
    connStr <- mkConnStr db
    result <- 
        session "google.com"
                (Just (const [plain (botUsername account) 
                                    Nothing 
                                    (botPassword account)], Nothing))
                def { sessionStreamConfiguration = def 
                        { tlsParams = defaultParamsClient 
                            { pConnectVersion = TLS10
                            , pAllowedVersions = [TLS10, TLS11, TLS12]
                            , pCiphers = ciphersuite_medium } } }
    sess <- case result of
                Right s -> log  "Session created." >> return s
                Left e -> log "Session Failed." >> 
                          error ("XmppFailure: " ++ show e)
    sendPresence def sess

    -- If contact is offline, terminate!
    presence <- timeout 10000000 $ waitForPresence 
        (\p -> fmap toBare (presenceFrom p) == Just contactJid) sess

    when (fmap presenceType presence == Just Available) $ do
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
                        log (name f ++ " fetched: " ++ show (length toSend))
                        let longMsg = T.unlines $ map pprFeed toSend
                            payload = length $ T.unpack longMsg
                        when (payload /= 0) $ do
                            let header = simpleIM contactJid (T.pack $ name f ++ ": " ++ show payload)
                            void $ sendMessage header sess 
                            let reply = simpleIM contactJid longMsg
                            void $ sendMessage reply sess 
                        log (name f ++ " sended.")

        -- Insert twitter status to DB if not exist
        status <- getTwitter twitter
        log ("Twitter timeline fetched: " ++ show (length status))
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
        log "Twitter timeline sended."
    rmLogger logger
    sendPresence presenceOffline sess 
    endSession sess
