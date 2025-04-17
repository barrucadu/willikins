module Willikins.LLM
  ( defaultLLM
  , module Willikins.LLM.Client
  , module Willikins.LLM.Tools
  , module Willikins.LLM.Types
  ) where

import Willikins.LLM.Client
import Willikins.LLM.Tools
import Willikins.LLM.Types

import Willikins.Memory (Connection)

-- | Default values for all parameters
defaultLLM :: Credentials -> Connection -> LLM
defaultLLM c db = LLM
  { llmModel = Claude37SonnetLatest
  , llmCredentials = c
  , llmTools = allTools db
  , llmMaxTokens = 5000
  }
