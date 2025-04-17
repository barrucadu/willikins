{-# LANGUAGE OverloadedStrings #-}

module Willikins.LLM.Tools
  ( allTools
  -- * Memory tools
  , createMemory
  , deleteMemory
  , doCreateMemory
  , doDeleteMemory
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import Willikins.LLM.Types
import Willikins.Memory

allTools :: Connection -> [(Tool, A.Value -> IO (Either String String))]
allTools db =
  [ (createMemory, doCreateMemory db)
  , (deleteMemory, doDeleteMemory db)
  ]

-------------------------------------------------------------------------------

-- | Schema for the `create_memory` tool.
createMemory :: Tool
createMemory = Tool
  { tName = "create_memory"
  , tDescription = "Commit a piece of information to memory so that you can recall and reference it later."
  , tArguments =
    [ ToolArgument
      { taName = "text"
      , taType = "string"
      , taDescription = "The information to note down, keep it short but include all important details."
      , taRequired = True
      }
    , ToolArgument
      { taName = "date"
      , taType = "string"
      , taDescription = "The actual date of the event or reminder.  Set a date whenever possible, but if no date is relevant leave this out."
      , taRequired = False
      }
    ]
  }

-- | Returns `Left` if the `text` parameter isn't given.  Doesn't validate the
-- format of the `date` parameter.
doCreateMemory :: Connection -> A.Value -> IO (Either String String)
doCreateMemory db = argshelper parser err act where
  parser v = (,) <$> v A..: "text" <*> v A..:? "date"
  err = Left "Missing required parameter 'text'"
  act val = Right . formatFactForLLM <$> insertFact db val

-- | Schema for the `delete_memory` tool
deleteMemory :: Tool
deleteMemory = Tool
  { tName = "delete_memory"
  , tDescription = "Delete a memory when it is no longer relevant."
  , tArguments =
    [ ToolArgument
      { taName = "id"
      , taType = "integer"
      , taDescription = "ID of the memory to delete.  Each memory is displayed with its ID in the format '[ID: 123]'."
      , taRequired = True
      }
    ]
  }

-- | Returns `Left` if the `id` parameter isn't given.  Doesn't valiate that
-- there actually is a memory with that ID.
doDeleteMemory :: Connection -> A.Value -> IO (Either String String)
doDeleteMemory db = argshelper parser err act where
  parser v = v A..: "id"
  err = Left "Missing required parameter 'id'"
  act val = Right ("Deleted memory [ID: " ++ show val ++ "]") <$ deleteFact db val

-------------------------------------------------------------------------------

argshelper :: (A.Object -> A.Parser a) -> x -> (a -> IO x) -> A.Value -> IO x
argshelper parser err f = maybe (pure err) f . A.parseMaybe (A.withObject "args" parser)
