{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Willikins.Memory
  ( Connection
  , withDatabase
  -- * Facts
  , Fact(..)
  , formatFactForLLM
  , insertFact
  , getAllFacts
  , deleteFact
  -- * Calendar events
  , Event(..)
  , formatEventForLLM
  , getAllEvents
  , replaceAllEvents
  -- * Conversations
  , ChatMessage(..)
  , getAllChats
  , getChatHistory
  , insertChatMessages
  -- * Feeds
  , Feed(..)
  , formatFeedForLLM
  , insertFeed
  , getAllFeeds
  , deleteFeedByURL
  , FeedEntry(..)
  , insertFeedEntries
  , getRandomUnreadFeedEntry
  , markFeedEntryAsRead
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple

import qualified Willikins.Integration.Feed as Feed
import qualified Willikins.LLM.Types as LLM

withDatabase :: String -> (Connection -> IO a) -> IO a
withDatabase dbname action = withConnection dbname $ \conn -> do
  initialiseDatabase conn
  action conn

initialiseDatabase :: Connection -> IO ()
initialiseDatabase conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS facts (id INTEGER PRIMARY KEY NOT NULL, text TEXT NOT NULL, date TEXT, created_at TEXT NOT NULL, deleted_at TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS events (id INTEGER PRIMARY KEY NOT NULL, text TEXT NOT NULL, created_at TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS chat_messages (id INTEGER PRIMARY KEY NOT NULL, chat_id TEXT NOT NULL, json TEXT NOT NULL, created_at TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS feeds (id INTEGER PRIMARY KEY NOT NULL, title TEXT NOT NULL, url TEXT NOT NULL, created_at TEXT NOT NULL, deleted_at TEXT)"
  execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS idx_feeds_url ON feeds(url)"
  execute_ conn "CREATE TABLE IF NOT EXISTS feed_entries (id INTEGER PRIMARY KEY NOT NULL, title TEXT NOT NULL, url TEXT NOT NULL, text TEXT NOT NULL, created_at TEXT NOT NULL, deleted_at TEXT)"
  execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS idx_feed_entries_url ON feed_entries(url)"

----------------------------------------------------------------------

-- | A record in Willikins' memory
data Fact = Fact
  { fId :: Int
  , fText :: String
  , fDate :: Maybe String
  , fCreatedAt :: UTCTime
  , fDeletedAt :: Maybe UTCTime
  }
  deriving Show

instance FromRow Fact where
  fromRow = Fact <$> field <*> field <*> field <*> field <*> field

-- | '{date} [ID: {id}]: {text}'
formatFactForLLM :: Fact -> String
formatFactForLLM Fact{..} = datespec ++ idspec ++ " " ++ fText where
  datespec = maybe "" (++" ") fDate
  idspec = "[ID: " ++ show fId ++ "]"

-- | Insert a new fact into the database, returning it.
insertFact :: Connection -> (String, Maybe String) -> IO Fact
insertFact conn (newText, newDate) = do
  now <- getCurrentTime
  execute conn "INSERT INTO facts (text, date, created_at) VALUES (?,?,?)" (newText, newDate, now)
  rowId <- lastInsertRowId conn
  r <- query conn "SELECT * FROM facts WHERE id = ?" (Only rowId)
  pure (head r)

-- | Get all facts
getAllFacts :: Connection -> IO [Fact]
getAllFacts conn = query_ conn "SELECT * FROM facts WHERE deleted_at IS NULL"

-- | Delete a fact
deleteFact :: Connection -> Int -> IO ()
deleteFact conn fId = do
  now <- getCurrentTime
  execute conn "UPDATE facts SET deleted_at = ? WHERE id = ?" (now, fId)

-------------------------------------------------------------------------------

-- | An event from the master's calendar.
data Event = Event
  { eId :: Int
  , eText :: String
  , eCreatedAt :: UTCTime
  }
  deriving Show

instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field

-- | '{text}'
formatEventForLLM :: Event -> String
formatEventForLLM = eText

-- | Get all events
getAllEvents :: Connection -> IO [Event]
getAllEvents conn = query_ conn "SELECT * FROM events"

-- | Transactionally replace the events in the database.
replaceAllEvents :: Connection -> [String] -> IO ()
replaceAllEvents conn events = withTransaction conn $ do
  now <- getCurrentTime
  execute_ conn "DELETE FROM events"
  executeMany conn "INSERT INTO events (text, created_at) VALUES (?,?)" [(e, now) | e <- events]

-------------------------------------------------------------------------------

-- | An entry in a conversation.
data ChatMessage = ChatMessage
  { cmId :: Int
  , cmChatId :: String
  , cmJSON :: BL.ByteString
  , cmCreatedAt :: UTCTime
  }
  deriving Show

instance FromRow ChatMessage where
  fromRow = ChatMessage <$> field <*> field <*> field <*> field

-- | Get all chat messages from all conversations, in ascending order of timestamp
getAllChats :: Connection -> IO [ChatMessage]
getAllChats conn = query_ conn "SELECT * FROM chat_messages ORDER BY created_at ASC, id ASC"

-- | Get recent chat messages (up to a limit), in ascending order of timestamp
getChatHistory :: Connection -> String -> Int -> IO [LLM.Message]
getChatHistory conn chatId limit = map (either error id . A.eitherDecode . cmJSON) . reverse <$>
  query conn "SELECT * FROM chat_messages WHERE chat_id = ? ORDER BY created_at DESC, id DESC LIMIT ?" (chatId, limit)

-- | Insert a collection of messages
insertChatMessages :: Connection -> String -> [LLM.Message] -> IO ()
insertChatMessages conn chatId msgs = withTransaction conn $ do
  now <- getCurrentTime
  let values = [(chatId, A.encode m, now) | m <- msgs]
  executeMany conn "INSERT INTO chat_messages (chat_id, json, created_at) VALUES (?,?,?)" values

-------------------------------------------------------------------------------

-- | An RSS / Atom feed
data Feed = Feed
  { feId :: Int
  , feTitle :: String
  , feURL :: String
  , feCreatedAt :: UTCTime
  , feDeletedAt :: Maybe UTCTime
  }
  deriving Show

instance FromRow Feed where
  fromRow = Feed <$> field <*> field <*> field <*> field <*> field

-- | 'TITLE: {title}\nURL: {url}'
formatFeedForLLM :: Feed -> String
formatFeedForLLM Feed{..} = unlines ["TITLE: " ++ feTitle, "URL: " ++ feURL]

-- | Insert a new feed into the database, returning it.
insertFeed :: Connection -> (String, String) -> IO Feed
insertFeed conn (newTitle, newURL) = do
  now <- getCurrentTime
  execute conn "INSERT INTO feeds (title, url, created_at) VALUES (?,?,?)" (newTitle, newURL, now)
  rowId <- lastInsertRowId conn
  r <- query conn "SELECT * FROM feeds WHERE id = ?" (Only rowId)
  pure (head r)

-- | Get all feeds.
getAllFeeds :: Connection -> IO [Feed]
getAllFeeds conn = query_ conn "SELECT * FROM feeds WHERE deleted_at IS NOT NULL"

-- | Mark a feed as deleted.
deleteFeedByURL :: Connection -> String -> IO ()
deleteFeedByURL conn feURL = do
  now <- getCurrentTime
  execute conn "UPDATE feeds SET deleted_at = ? WHERE url = ?" (now, feURL)

-- | An RSS / Atom feed entry
data FeedEntry = FeedEntry
  { fenId :: Int
  , fenTitle :: String
  , fenURL :: String
  , fenText :: String
  , fenCreatedAt :: UTCTime
  , fenDeletedAt :: Maybe UTCTime
  }
  deriving Show

instance FromRow FeedEntry where
  fromRow = FeedEntry <$> field <*> field <*> field <*> field <*> field <*> field

-- | Insert a collection of feed entries, ignoring conflicts, returning the
-- number of new entries.
insertFeedEntries :: Connection -> [Feed.Entry] -> IO Int
insertFeedEntries conn entries = do
  now <- getCurrentTime
  let values = [(eTitle, eURL, fromMaybe "" eText, now) | Feed.Entry{..} <- entries]
  [Only before] <- query_ conn "SELECT COUNT(1) FROM feed_entries"
  executeMany conn "INSERT OR IGNORE INTO feed_entries (title, url, text, created_at) VALUES (?,?,?,?)" values
  [Only after] <- query_ conn "SELECT COUNT(1) FROM feed_entries"
  pure $ after - before

-- | Get a single random unread entry.
getRandomUnreadFeedEntry :: Connection -> IO (Maybe FeedEntry)
getRandomUnreadFeedEntry conn = do
  r <- query_ conn "SELECT * FROM feed_entries WHERE deleted_at IS NULL ORDER BY RANDOM() LIMIT 1"
  pure $ case r of (fe:_) -> Just fe; [] -> Nothing

-- | Mark an entry as read / deleted.
markFeedEntryAsRead :: Connection -> Int -> IO ()
markFeedEntryAsRead conn fenId = do
  now <- getCurrentTime
  execute conn "UPDATE feed_entries SET deleted_at = ? WHERE id = ?" (now, fenId)
