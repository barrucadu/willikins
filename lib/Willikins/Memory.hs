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
  )
where

import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple

withDatabase :: String -> (Connection -> IO a) -> IO a
withDatabase dbname action = withConnection dbname $ \conn -> do
  initialiseDatabase conn
  action conn

initialiseDatabase :: Connection -> IO ()
initialiseDatabase conn =
  execute_ conn "CREATE TABLE IF NOT EXISTS facts (id INTEGER PRIMARY KEY NOT NULL, text TEXT NOT NULL, date TEXT, created_at TEXT NOT NULL, deleted_at TEXT)"

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
  execute conn "UPDATE FACTS SET deleted_at = ? WHERE id = ?" (now, fId)
