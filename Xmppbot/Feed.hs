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
import           Network.OAuth.Consumer (Token)
import           Text.Feed.Import (parseFeedString)
import qualified Text.Feed.Types as F
import           Text.Feed.Query ( feedItems, getItemTitle
                                 , getItemPublishDateString, getItemLink)
import qualified Web.Twitter as TT
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
             <*> getItemPublishDateString item
             <*> getItemLink item
             <*> Just contact

pprFeed :: FeedItem -> Text
pprFeed item = T.pack $
    "[" ++ feedItemSource item ++ "]: "
        ++ feedItemTitle item ++ " " ++ feedItemLink item

makeStatus :: String -> TT.Status -> FeedItem
makeStatus contact st = FeedItem ('@' : TT.user st) (TT.text st) (TT.id_str st) "" contact

getHomeTL :: Token -> String -> IO [FeedItem]
getHomeTL tok contact = do
    st <- TT.homeTimeline tok []
    return $ map (makeStatus contact) st

getUserTL :: Token -> String -> String -> IO [FeedItem]
getUserTL tok uname contact = do
    st <- TT.authUserTimeline tok uname []
    return $ map (makeStatus contact) st

