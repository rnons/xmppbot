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
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Time (ZonedTime)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Network.TLS (Params(pConnectVersion, pAllowedVersions, pCiphers), Version(TLS10, TLS11, TLS12), defaultParamsClient)
import           Network.TLS.Extra (ciphersuite_medium)
import           Network.Xmpp
import           Network.Xmpp.IM (simpleIM)
import           Text.Feed.Import (parseFeedString)
import qualified Text.Feed.Types as F
import           Text.Feed.Query (feedItems, getItemTitle, getItemPublishDate, getItemLink)
import qualified Web.Twitter as TT
import           Web.Twitter.OAuth (Consumer(..), singleAccessToken)

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
TwitterStatus
    user String
    text String
    id_str String
    UniqueStatus id_str
    deriving Show
|]

mkConnStr :: Result Database -> IO C.ByteString
mkConnStr (Success s) = return $ C.pack $ "host=" <> host s <>
               " dbname=" <> database s <>
               " user=" <> user s <>
               " password=" <> password s <>
               " port=" <> show (port s)
mkConnStr (Error err) = error $ "mkConnStr failed: " ++ show err

getFeedItems :: Result [Feeds] -> IO [FeedItem]
getFeedItems (Success feeds) = fmap concat $ mapM getFeed feeds
getFeedItems (Error err) = error $ "Parse feedlist failed: " ++ show err

getFeed :: Feeds -> IO [FeedItem]
getFeed feedsource = do
    result <- simpleHttp $ url feedsource
    let feed = parseFeedString $ LC.unpack result
    let items = case feed of
                    Just f -> fromMaybe [] $ mapM (makeItem $ name feedsource) (feedItems f)
                    Nothing -> []
    return items

-- | Make an item from a feed item.
makeItem :: String -> F.Item -> Maybe FeedItem
makeItem src item =
    FeedItem <$> Just src
             <*> getItemTitle item
             <*> getItemPublishDate item
             <*> getItemLink item

getTwitter :: Result Twitter -> IO [TwitterStatus]
getTwitter (Success s) = do
    let consumer = Consumer (consumerKey s) (consumerSecret s)
    tok <- singleAccessToken consumer (oauthToken s) (oauthTokenSecret s) 
    st <- TT.homeTimeline tok []
    return $ map makeStatus st
  where
    makeStatus st = TwitterStatus (TT.user st) (TT.text st) (TT.id_str st)
getTwitter (Error err) = error $ "getTwitter failed :" ++ show err

pprFeed :: FeedItem -> Text
pprFeed item = T.pack $
    "[" ++ feedItemSource item ++ "]: " 
        ++ feedItemTitle item ++ " <" ++ feedItemLink item ++ ">"

pprTwitter :: TwitterStatus -> Text
pprTwitter st = T.pack $
    "[@" ++ twitterStatusUser st ++ "]: " ++ twitterStatusText st

loadYaml :: String -> IO (M.HashMap Text Value)
loadYaml fp = do
    mval <- decodeFile fp
    case mval of
      Nothing  -> error $ "Invalid YAML file: " ++ show fp
      Just obj -> return obj

parseYaml :: FromJSON a => Text -> (M.HashMap Text Value) -> Result a
parseYaml key hm =
    case M.lookup key hm of
        Just val -> fromJSON val
        Nothing  -> Error $ "Failed to load " ++ T.unpack key

main :: IO ()
main = do
    config <- loadYaml "bot.yml"
    let db = parseYaml "Database" config :: Result Database
        account = parseYaml "Account" config :: Result Account
        twitter = parseYaml "Twitter" config :: Result Twitter
        feeds = parseYaml "Feeds" config :: Result [Feeds]

    connStr <- mkConnStr db
    (botName, botWord, contact) <-
        case account of
            Success acc -> return (botUsername acc, botPassword acc, contactUsername acc)
            Error err -> error $ "Parse account failed:" ++ show err

    result <- session
                 "google.com"
                  (Just (\_ -> ( [plain botName Nothing botWord])
                               , Nothing))
                  def { 
                      sessionStreamConfiguration = def { 
                          tlsParams = defaultParamsClient { 
                              pConnectVersion = TLS10, 
                              pAllowedVersions = [TLS10, TLS11, TLS12], 
                              pCiphers = ciphersuite_medium 
                              } 
                          } 
                      }
    sess <- case result of
                Right s -> return s
                Left e -> error $ "XmppFailure: " ++ show e
    sendPresence def sess
    presence <- waitForPresence (\p ->
        fmap toBare (presenceFrom p) == jidFromText contact) sess
    when (presenceType presence == Available) $ do
        items <- getFeedItems feeds
        status <- getTwitter twitter
        withPostgresqlPool connStr 10 $ \pool -> do
            flip runSqlPersistMPool pool $ do
                runMigration migrateAll

                -- Generic feeds
                forM_ items $ \j -> do
                    ent <- insertBy j
                    case ent of
                        Left (Entity k v) -> liftIO $ return ()
                        Right _ -> do
                            let reply = simpleIM (parseJid $ T.unpack contact) (pprFeed j)
                            void $ liftIO $ sendMessage reply sess 

                -- Twitter timeline
                forM_ status $ \j -> do
                    ent <- insertBy j
                    case ent of
                        Left (Entity k v) -> liftIO $ return ()
                        Right _ -> do
                            let reply = simpleIM (parseJid $ T.unpack contact) (pprTwitter j)
                            void $ liftIO $ sendMessage reply sess 
