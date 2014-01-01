{-# LANGUAGE DeriveGeneric #-}
module Xmppbot.Feed where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Text.Feed.Import (parseFeedString)
import qualified Text.Feed.Types as F
import           Text.Feed.Query ( feedItems, getItemTitle
                                 , getItemPublishDate, getItemLink)
import qualified Web.Twitter as TT
import           Web.Twitter.OAuth (Consumer(..), singleAccessToken)
import Model

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

getFeed :: String -> Feeds -> IO [FeedItem]
getFeed contact feedsource = do
    result <- simpleHttp $ url feedsource
    let feed = parseFeedString $ LC.unpack result
    case feed of
        Just f  -> return $ fromMaybe [] $
                   mapM (makeItem contact $ name feedsource) (feedItems f)
        Nothing -> return []

-- | Make an item from a feed item.
makeItem :: String -> String -> F.Item -> Maybe FeedItem
makeItem contact src item =
    FeedItem <$> Just src
             <*> getItemTitle item
             <*> getItemPublishDate item
             <*> getItemLink item
             <*> Just contact

pprFeed :: FeedItem -> Text
pprFeed item = T.pack $
    "[" ++ feedItemSource item ++ "]: "
        ++ feedItemTitle item ++ " " ++ feedItemLink item

getTwitter :: String -> Twitter -> IO [FeedItem]
getTwitter contact s = do
    let consumer = Consumer (consumerKey s) (consumerSecret s)
    tok <- singleAccessToken consumer (oauthToken s) (oauthTokenSecret s)
    st <- TT.homeTimeline tok []
    return $ map makeStatus st
  where
    makeStatus st = FeedItem ('@' : TT.user st) (TT.text st) (TT.id_str st) "" contact

