{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Foldable (traverse_)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import qualified Data.Time.Format as TF
import System.IO (getLine, putStrLn)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Willikins.Integration.GoogleCalendar as GCal
import Willikins.LLM.Client
import qualified Willikins.Memory as Mem

main :: IO ()
main = Mem.withDatabase "willikins.sqlite" $ \db -> do
  events <- GCal.fetchEvents =<< GCal.credentialsFromEnvironment
  facts <- Mem.getAllFacts db
  now <- getCurrentTime
  let sysPrompt = systemPrompt events now facts
  getArgs >>= \case
    ["debug", "dump-system-prompt"] -> putStrLn sysPrompt
    ["debug", "dump-facts"] -> traverse_ print facts
    _ -> credentialsFromEnvironment >>= chatbot sysPrompt db

chatbot :: String -> Mem.Connection -> Credentials -> IO ()
chatbot sysPrompt db c = go initialHistory where
  go history = oneshot (demoTools db) c sysPrompt history >>= \case
    Right history' -> do
      traverse_ (\m -> printMessage m >> putStrLn sep) history'
      query <- getQuery
      putStrLn sep
      let user = MessageText { mtRole = User, mtText = query }
      go (history ++ history' ++ [user])
    Left err -> die err

  printMessage MessageText{..} = putStrLn mtText
  printMessage MessageToolUse{..} = do
    putStrLn $ "TOOL USE: " ++ mtuId ++ " [" ++ mtuTool ++ "]"
    print mtuInput
  printMessage MessageToolResult{..} = do
    putStrLn $ "TOOL RESPONSE: " ++ mtrId
    print mtrText

  initialHistory = [MessageText { mtRole = Assistant, mtText = "" }]

getQuery :: IO String
getQuery = go [] where
  go ls = do
    l <- getLine
    if l == "."
      then pure (unlines (reverse ls))
      else go (l:ls)

demoTools :: Mem.Connection -> [(Tool, A.Value -> IO (Either String String))]
demoTools db = [(createMemory, doCreateMemory), (deleteMemory, doDeleteMemory)] where
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

  doCreateMemory args = case A.parseMaybe (A.withObject "args" $ \v -> (,) <$> v A..: "text" <*> v A..:? "date") args of
    Just val -> Right . Mem.formatFactForLLM <$> Mem.insertFact db val
    Nothing -> pure $ Left "Missing required parameter 'text'"

  doDeleteMemory args = case A.parseMaybe (A.withObject "args" $ \v -> v A..: "id") args of
    Just val -> Right ("Deleted memory [ID: " ++ show val ++ "]") <$ Mem.deleteFact db val
    Nothing -> pure $ Left "Missing required parameter 'id'"

systemPrompt :: [GCal.Event] -> UTCTime -> [Mem.Fact] -> String
systemPrompt events now facts = unlines prompt where
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
    , ""
    ] ++ map showEvent events ++
    [ ""
    , "The current date is " ++ today
    , ""
    , "Before responding to the user, use the create_memory tool to record any reminders or any other information that might be useful to reference later."
    , "You do not need to commit the calendar entries to memory."
    , ""
    , "You have previously committed the following facts to memory:"
    ] ++ map showFact facts

  showEvent e = "- " ++ GCal.formatEventForLLM e

  showFact f = "- " ++ Mem.formatFactForLLM f

  today = TF.formatTime TF.defaultTimeLocale "%A, %Y-%m-%d" now

sep :: String
sep = unlines
  [ ""
  , "-----"
  ]

die :: Error -> IO a
die err = do
  putStrLn "My humble apologies sir, I have encountered a problem, namely:"
  putStrLn ""
  putStrLn $ case err of
    ApiError eType eMessage -> "    API Error '" ++ eType ++ "': " ++ eMessage
    InvalidApiResponseError raw -> "    Invalid API Response: " ++ raw
    NetworkError -> "    Network Error"
  putStrLn ""
  putStrLn "I'll go have a lie down, but please do not hesitate to call upon me."
  exitFailure
