{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Willikins.LLM.Types
  ( Model(..)
  , MessagesRequest(..)
  , Message(..)
  , Role(..)
  , Tool(..)
  , ToolArgument(..)
  , MessagesResponse(..)
  , MessagesResponseContent(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as A
import Data.Char (toLower)
import Data.Either (isLeft)
import Data.String (fromString)
import GHC.Generics (Generic)

data Model = Claude37SonnetLatest
  deriving Show

instance ToJSON Model where
  toJSON Claude37SonnetLatest = "claude-3-7-sonnet-latest"

-------------------------------------------------------------------------------

-- | https://docs.anthropic.com/en/api/messages
data MessagesRequest = MessagesRequest
  { maxTokens :: Integer
  , messages :: [Message]
  , model :: Model
  , system :: String
  , tools :: [Tool]
  }
  deriving (Generic, Show)

instance ToJSON MessagesRequest where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' }

-- | https://docs.anthropic.com/en/api/messages#body-messages
data Message
  = MessageText
    { mtRole :: Role
    , mtText :: String
    }
  | MessageToolUse
    { mtuId :: String
    , mtuTool :: String
    , mtuInput :: A.Value
    }
  | MessageToolResult
    { mtrId :: String
    , mtrText :: Either String String
    }
  deriving Show

instance ToJSON Message where
  toJSON t = A.object [ "role" A..= role, "content" A..= content ] where
    role = case t of
      MessageText{..} -> mtRole
      MessageToolUse{} -> Assistant
      MessageToolResult{} -> User

    content = case t of
      MessageText{..}
        | null mtText -> []
        | otherwise ->
          [ A.object
            [ "type" A..= ("text" :: String)
            , "text" A..= mtText
            ]
          ]
      MessageToolUse{..} ->
        [ A.object
          [ "type" A..= ("tool_use" :: String)
          , "id" A..= mtuId
          , "name" A..= mtuTool
          , "input" A..= mtuInput
          ]
        ]
      MessageToolResult{..} ->
        [ A.object
          [ "type" A..= ("tool_result" :: String)
          , "tool_use_id" A..= mtrId
          , "content" A..= either id id mtrText
          , "is_error" A..= isLeft mtrText
          ]
        ]

-- only needs to be consistent with the ToJSON instance, not with the API
-- response format in general
instance FromJSON Message where
  parseJSON = A.withObject "Message" $ \v -> do
      role <- v A..: "role"
      content <- v A..: "content"
      case content of
        [] -> pure $ MessageText { mtRole = role, mtText = "" }
        [x] -> parseContent role x
        xs -> inconsistency "message should only have one content block" (show xs)
    where
      parseContent role = A.withObject "Content" $ \v -> do
        ty <- v A..: "type"
        case ty :: String of
          "text" -> parseText role v
          "tool_use" -> parseToolUse v
          "tool_result" -> parseToolResult v
          _gg -> inconsistency "unknown type" ty

      parseText role v = MessageText role <$> v A..: "text"

      parseToolUse v = MessageToolUse <$> v A..: "id" <*> v A..: "name" <*> v A..: "input"

      parseToolResult v = do
        tuId <- v A..: "tool_use_id"
        tuText <- v A..: "content"
        tuErr <- v A..: "is_error"
        pure $
          if tuErr
          then MessageToolResult { mtrId = tuId, mtrText = Left tuText }
          else MessageToolResult { mtrId = tuId, mtrText = Right tuText }

      inconsistency msg val = error $ "ToJSON / FromJSON inconsistency - " ++ msg ++ ": " ++ val

-- | https://docs.anthropic.com/en/api/messages#body-messages-role
data Role = User | Assistant
  deriving (Generic, Show)

instance ToJSON Role where
  toJSON = A.genericToJSON A.defaultOptions { A.constructorTagModifier = map toLower }

instance FromJSON Role where
  parseJSON = A.genericParseJSON A.defaultOptions { A.constructorTagModifier = map toLower }

-- | https://docs.anthropic.com/en/api/messages#body-tools
data Tool = Tool
  { tName :: String
  , tDescription :: String
  , tArguments :: [ToolArgument]
  }
  deriving Show

instance ToJSON Tool where
  toJSON t = json where
    json = A.object
      [ "name" A..= tName t
      , "description" A..= tDescription t
      , "input_schema" A..= A.object
        [ "type" A..= ("object" :: String)
        , "properties" A..= A.object (map toolArgumentJSON (tArguments t))
        , "required" A..= [taName ta | ta <- tArguments t, taRequired ta]
        ]
      ]

    toolArgumentJSON ta = fromString (taName ta) A..= A.object
      [ "type" A..= taType ta
      , "description" A..= taDescription ta
      ]

-- | https://docs.anthropic.com/en/api/messages#body-tools
data ToolArgument = ToolArgument
  { taName :: String
  , taType :: String
  , taDescription :: String
  , taRequired :: Bool
  }
  deriving Show

-- | https://docs.anthropic.com/en/api/messages#response-content
newtype MessagesResponse = MessagesResponse
  { mrContent :: [MessagesResponseContent]
  }
  deriving (Generic, Show)

instance FromJSON MessagesResponse where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' . drop 2 }

data MessagesResponseContent
  = MessagesResponseContentText
    { mrctText :: String
    }
  | MessagesResponseContentToolUse
    { mrctuId :: String
    , mrctuTool :: String
    , mrctuInput :: A.Value
    }
  deriving Show

instance FromJSON MessagesResponseContent where
  parseJSON = A.withObject "Content" $ \v -> do
      ty <- v A..: "type"
      if ty == ("tool_use" :: A.Value) then parseToolUse v else parseText v
    where
      parseToolUse v =
        MessagesResponseContentToolUse <$> v A..: "id" <*> v A..: "name" <*> v A..: "input"

      parseText v =
        MessagesResponseContentText <$> v A..: "text"
