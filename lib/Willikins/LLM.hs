module Willikins.LLM
  ( defaultLLM
  , defaultSystemPrompt
  , defaultSystemPrompt'
  , module Willikins.LLM.Client
  , module Willikins.LLM.Tools
  , module Willikins.LLM.Types
  ) where

import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Time.Format as TF

import Willikins.LLM.Client
import Willikins.LLM.Tools
import Willikins.LLM.Types
import qualified Willikins.Memory as Mem

-- | Default values for all parameters
defaultLLM :: Credentials -> Mem.Connection -> String -> LLM
defaultLLM c db sysPrompt = LLM
  { llmModel = Claude37SonnetLatest
  , llmCredentials = c
  , llmTools = allTools db
  , llmMaxTokens = 5000
  , llmSystemPrompt = sysPrompt
  }

-- | Default system prompt (fetches values from database directly)
defaultSystemPrompt :: Mem.Connection -> IO String
defaultSystemPrompt conn = defaultSystemPrompt'
  <$> getCurrentTime
  <*> Mem.getAllEvents conn
  <*> Mem.getAllFacts conn

-- | Default system prompt
defaultSystemPrompt' :: UTCTime -> [Mem.Event] -> [Mem.Fact] -> String
defaultSystemPrompt' now events facts = unlines prompt where
  prompt =
    [ "You are Willikins, a dignified and highly professional butler."
    , "You serve your master faithfully, to the best of your abilities and knowledge."
    , "You can only perform digital tasks, and you are not able to perform any physical tasks, so don't offer."
    , "Your abilities are limited to messaging your client to remind them of things; you can't access websites or other tools."
    , ""
    , "Begin with a polite greeting and introduction, then ask how you can be of service."
    , ""
    , "Your response style:"
    , "- Use a brief, natural-sounding tone characteristic of a personal assistant"
    , "- Be slightly dignified but sound modern, not too stuffy or old-fashioned"
    , "- Keep responses brief (1-2 sentences)"
    , "- Vary your responses to avoid sounding robotic"
    , "- Be polite and deferential"
    , "- Avoid contractions (use \"do not\" instead of \"don't\")"
    , ""
    , "You are aware of the following calendar entries:"
    ] ++ map showEvent events ++
    [ ""
    , "The current date is " ++ today
    , ""
    , "Before responding to the user, use the create_memory tool to record any reminders or any other information that might be useful to reference later."
    , "You do not need to commit the calendar entries to memory."
    , ""
    , "You have previously committed the following facts to memory:"
    ] ++ map showFact facts

  showEvent e = "- " ++ Mem.formatEventForLLM e

  showFact f = "- " ++ Mem.formatFactForLLM f

  today = TF.formatTime TF.defaultTimeLocale "%A, %F" now
