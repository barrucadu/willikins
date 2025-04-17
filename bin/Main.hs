{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import qualified Data.Time.Format as TF
import System.IO (getLine, putStrLn)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Willikins.Integration.GoogleCalendar as GCal
import qualified Willikins.LLM as LLM
import qualified Willikins.Memory as Mem

main :: IO ()
main = Mem.withDatabase "willikins.sqlite" $ \db -> do
  credentials <- LLM.credentialsFromEnvironment
  events <- GCal.fetchEvents =<< GCal.credentialsFromEnvironment
  facts <- Mem.getAllFacts db
  now <- getCurrentTime
  let sysPrompt = systemPrompt events now facts
  getArgs >>= \case
    ["debug", "dump-system-prompt"] -> putStrLn sysPrompt
    ["debug", "dump-facts"] -> traverse_ print facts
    _ -> chatbot sysPrompt $ LLM.defaultLLM credentials db

chatbot :: String -> LLM.LLM -> IO ()
chatbot sysPrompt llm = go initialHistory where
  go history = LLM.oneshot llm sysPrompt history >>= \case
    Right history' -> do
      traverse_ (\m -> printMessage m >> putStrLn sep) history'
      query <- getQuery
      putStrLn sep
      let user = LLM.MessageText { mtRole = LLM.User, mtText = query }
      go (history ++ history' ++ [user])
    Left err -> die err

  printMessage LLM.MessageText{..} = putStrLn mtText
  printMessage LLM.MessageToolUse{..} = do
    putStrLn $ "TOOL USE: " ++ mtuId ++ " [" ++ mtuTool ++ "]"
    print mtuInput
  printMessage LLM.MessageToolResult{..} = do
    putStrLn $ "TOOL RESPONSE: " ++ mtrId
    print mtrText

  initialHistory = [LLM.MessageText { mtRole = LLM.Assistant, mtText = "" }]

getQuery :: IO String
getQuery = go [] where
  go ls = do
    l <- getLine
    if l == "."
      then pure (unlines (reverse ls))
      else go (l:ls)

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

die :: LLM.Error -> IO a
die err = do
  putStrLn "My humble apologies sir, I have encountered a problem, namely:"
  putStrLn ""
  putStrLn $ case err of
    LLM.ApiError eType eMessage -> "    API Error '" ++ eType ++ "': " ++ eMessage
    LLM.InvalidApiResponseError raw -> "    Invalid API Response: " ++ raw
    LLM.NetworkError -> "    Network Error"
  putStrLn ""
  putStrLn "I'll go have a lie down, but please do not hesitate to call upon me."
  exitFailure
