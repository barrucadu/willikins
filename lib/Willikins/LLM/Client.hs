{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Willikins.LLM.Client
  ( Model(..)
  , defaultModel
  -- * Authentication
  , Credentials
  , credentialsFromEnvironment
  -- * Errors
  , Error(..)
  -- * Messages API
  , MessagesRequest(..)
  , Message(..)
  , Role(..)
  , postMessages
  ) where

import qualified Control.Exception.Safe as E
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as A
import Data.String (fromString)
import GHC.Generics (Generic)
import qualified Network.HTTP.Simple as H
import System.Environment (getEnv)

data Model = Claude37SonnetLatest
  deriving Show

instance ToJSON Model where
  toJSON Claude37SonnetLatest = "claude-3-7-sonnet-latest"

defaultModel :: Model
defaultModel = Claude37SonnetLatest

-------------------------------------------------------------------------------

newtype Credentials = Credentials
  { apiKey :: String
  }

-- | Read the "ANTHROPIC_API_KEY" environment variable
credentialsFromEnvironment :: IO Credentials
credentialsFromEnvironment = Credentials <$> getEnv "ANTHROPIC_API_KEY"

-------------------------------------------------------------------------------

data Error
  = ApiError { eType :: String, eMessage :: String }
  | InvalidApiResponseError String
  | NetworkError

instance FromJSON Error where
  parseJSON = A.withObject "ApiError" $ \v -> do
    errv <- v A..: "error"
    ApiError
      <$> errv A..: "type"
      <*> errv A..: "message"

-------------------------------------------------------------------------------

-- | https://docs.anthropic.com/en/api/messages
data MessagesRequest = MessagesRequest
  { maxTokens :: Integer
  , messages :: [Message]
  , model :: Model
  , system :: String
  }
  deriving (Generic, Show)

instance ToJSON MessagesRequest where
  toEncoding = A.genericToEncoding A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' }

-- | https://docs.anthropic.com/en/api/messages#body-messages
data Message = Message
  { role :: Role
  , content :: String
  }
  deriving (Generic, Show)

instance ToJSON Message where
  toEncoding = A.genericToEncoding A.defaultOptions

-- | https://docs.anthropic.com/en/api/messages#body-messages-role
data Role = User | Assistant
  deriving Show

instance ToJSON Role where
  toJSON User = "user"
  toJSON Assistant = "assistant"

-- | https://docs.anthropic.com/en/api/messages#response-content
newtype MessagesResponse = MessagesResponse
  { mrContent :: [MessagesResponseContent]
  }
  deriving (Generic, Show)

instance FromJSON MessagesResponse where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' . drop 2 }

newtype MessagesResponseContent = MessagesResponseContent
  { mrcText :: String
  }
  deriving (Generic, Show)

instance FromJSON MessagesResponseContent where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' . drop 3 }

postMessages :: Credentials -> MessagesRequest -> IO (Either Error String)
postMessages credentials mr = flip fmap (http credentials req) $ \case
    Right (MessagesResponse { mrContent = [msg] }) -> Right (mrcText msg)
    Right msg -> Left (InvalidApiResponseError (show msg))
    Left err -> Left err
  where
    req = H.setRequestBodyJSON mr $ H.parseRequest_ "POST https://api.anthropic.com/v1/messages"

-------------------------------------------------------------------------------

http :: (FromJSON response) => Credentials -> H.Request -> IO (Either Error response)
http credentials req = do
  resp <- E.tryAny (H.getResponseBody <$> H.httpLBS req')
  pure $ case resp of
    Right body -> case A.decode body of
      Just x -> Right x
      Nothing -> maybe (Left (InvalidApiResponseError (show body))) Left $ A.decode body
    Left _ -> Left NetworkError
  where
    req' =
      H.setRequestHeader "x-api-key" [fromString $ apiKey credentials] $
      H.setRequestHeader "anthropic-version" ["2023-06-01"]
      req
