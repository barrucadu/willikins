{-# LANGUAGE RecordWildCards #-}

module Willikins.Integration.Feed
  ( FeedURL(..)
  , Entry(..)
  , fetchFeedEntries
  , fetchFeed
  ) where

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Network.HTTP.Simple as H
import qualified Text.Feed.Import as F
import qualified Text.Feed.Types as F
import qualified Text.Atom.Feed as AF
import qualified Text.RSS.Syntax as RF
import qualified Text.RSS1.Syntax as R1F

import Willikins.Utils

newtype FeedURL = FeedURL String

data Entry = Entry
  { eURL :: String
  , eTitle :: String
  , eText :: Maybe String
  }

-- | Given a list of URLs of known entries, return new ones
fetchFeedEntries :: FeedURL -> IO (Maybe [Entry])
fetchFeedEntries fURL = (toEntries =<<) <$> fetchFeed fURL

-- | Download a feed
fetchFeed :: FeedURL -> IO (Maybe F.Feed)
fetchFeed (FeedURL fURL) = (F.parseFeedSource =<<) <$> httpRequest req where
  req = H.parseRequest_ $ "GET " ++ fURL

-------------------------------------------------------------------------------
-- conversion functions

-- | Convert a feed into a list of `Entry`
toEntries :: F.Feed -> Maybe [Entry]
toEntries (F.AtomFeed feed) = Just $ atomFeedToEntries feed
toEntries (F.RSSFeed feed) = Just $ rssFeedToEntries feed
toEntries (F.RSS1Feed feed) = Just $ rss1FeedToEntries feed
toEntries (F.XMLFeed _) = Nothing

atomFeedToEntries :: AF.Feed -> [Entry]
atomFeedToEntries AF.Feed{..} = map toEntry feedEntries where
  toEntry AF.Entry{..} = Entry
    { eURL = T.unpack entryId
    , eTitle = AF.txtToString entryTitle
    , eText = case (entryContent, entrySummary) of
        (Just (AF.TextContent txt), _) -> Just $ AF.txtToString (AF.TextString txt)
        (Just (AF.HTMLContent html), _) -> Just $ AF.txtToString (AF.HTMLString html)
        (Just (AF.XHTMLContent xhtml), _) -> Just $ AF.txtToString (AF.XHTMLString xhtml)
        (_, Just summary) -> Just $ AF.txtToString summary
        (_, _) -> Nothing
    }

rssFeedToEntries :: RF.RSS -> [Entry]
rssFeedToEntries RF.RSS { rssChannel = RF.RSSChannel{..} } = mapMaybe toEntry rssItems where
  toEntry RF.RSSItem{..} = do
    link <- rssItemLink
    title <- rssItemTitle
    pure $ Entry
      { eURL = T.unpack link
      , eTitle = T.unpack title
      , eText = T.unpack <$> rssItemDescription
      }

rss1FeedToEntries :: R1F.Feed -> [Entry]
rss1FeedToEntries R1F.Feed {..} = map toEntry feedItems where
  toEntry R1F.Item{..} = Entry
    { eURL = T.unpack itemURI
    , eTitle = T.unpack itemTitle
    , eText = T.unpack <$> itemDesc
    }
