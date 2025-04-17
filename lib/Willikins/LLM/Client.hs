{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Willikins.LLM.Client
  ( LLM(..)
  -- * Authentication
  , Credentials
  , credentialsFromEnvironment
  -- * Errors
  , Error(..)
  -- * Messages API
  , oneshot
  , postMessages
  ) where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Network.HTTP.Simple as H
import System.Environment (getEnv)

import Willikins.LLM.Types
import Willikins.Utils

data LLM = LLM
  { llmModel :: Model
  , llmCredentials :: Credentials
  , llmTools :: [(Tool, A.Value -> IO (Either String String))]
  , llmMaxTokens :: Integer
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
postMessages credentials mr = either (Left . mapError) Right <$> httpRequestJSON req where
  mapError Nothing = NetworkError
  mapError (Just bs) = fromMaybe (InvalidApiResponseError (show bs)) $ A.decode bs

  req =
    H.setRequestHeader "x-api-key" [fromString $ apiKey credentials] $
    H.setRequestHeader "anthropic-version" ["2023-06-01"] $
    H.setRequestBodyJSON mr $
    H.parseRequest_ "POST https://api.anthropic.com/v1/messages"
