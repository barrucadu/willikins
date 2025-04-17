{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Willikins.LLM.Client
  ( LLM(..)
  , defaultLLM
  -- * Authentication
  , Credentials
  , credentialsFromEnvironment
  -- * Errors
  , Error(..)
  -- * Messages API
  , oneshot
  , postMessages
  ) where

import qualified Control.Exception.Safe as E
import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.String (fromString)
import qualified Network.HTTP.Simple as H
import System.Environment (getEnv)

import Willikins.LLM.Types

data LLM = LLM
  { llmModel :: Model
  , llmCredentials :: Credentials
  , llmTools :: [(Tool, A.Value -> IO (Either String String))]
  , llmMaxTokens :: Integer
  }

-- | Default values for all parameters
defaultLLM :: Credentials -> LLM
defaultLLM c = LLM
  { llmModel = Claude37SonnetLatest
  , llmCredentials = c
  , llmTools = []
  , llmMaxTokens = 5000
  }

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

-- | Make a request to the LLM and return its response, resolving any tool use.
oneshot
  :: LLM
  -- ^ LLM client
  -> String
  -- ^ System prompt
  -> [Message]
  -- ^ Conversation history
  -> IO (Either Error [Message])
oneshot LLM{..} sysPrompt history0 = go [] where
  go history' = postMessages llmCredentials (req (history0 ++ history')) >>= \case
    Right resp -> handleResponse False history' (mrContent resp)
    Left err -> pure $ Left err

  handleResponse True history' [] = go history'
  handleResponse False history' [] = pure (Right history')
  handleResponse loop history' (MessagesResponseContentText{..}:ms) =
    let assistant = MessageText { mtRole = Assistant, mtText = mrctText }
    in handleResponse loop (history' ++ [assistant]) ms
  handleResponse _ history' (MessagesResponseContentToolUse{..}:ms) = do
    toolResponse <- handleTool mrctuTool mrctuInput
    let assistant = MessageToolUse { mtuId = mrctuId, mtuTool = mrctuTool, mtuInput = mrctuInput }
    let user = MessageToolResult { mtrId = mrctuId, mtrText = toolResponse }
    handleResponse True (history' ++ [assistant, user]) ms

  handleTool name input = handleTool' llmTools where
    handleTool' [] = pure $ Left ("No such tool '" ++ name ++ "'")
    handleTool' ((t, f):ts)
      | tName t == name = f input
      | otherwise = handleTool' ts

  req history = defaultReq { messages = history }

  defaultReq = MessagesRequest
    { maxTokens = llmMaxTokens
    , messages = []
    , model = llmModel
    , system = sysPrompt
    , tools = map fst llmTools
    }

-- | Make a single request to the LLM API
postMessages :: Credentials -> MessagesRequest -> IO (Either Error MessagesResponse)
postMessages credentials mr = http credentials req where
  req = H.setRequestBodyJSON mr $ H.parseRequest_ "POST https://api.anthropic.com/v1/messages"

-------------------------------------------------------------------------------

http :: FromJSON response => Credentials -> H.Request -> IO (Either Error response)
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
