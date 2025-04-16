{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Time.Format as TF
import System.IO (getLine, putStrLn)
import System.Exit (exitFailure)

import qualified Willikins.Integration.GoogleCalendar as GCal
import Willikins.LLM.Client

main :: IO ()
main = do
  credentials <- credentialsFromEnvironment
  events <- GCal.fetchEvents =<< GCal.credentialsFromEnvironment
  now <- getCurrentTime
  let sysPrompt = systemPrompt events now
  chatbot credentials sysPrompt

chatbot :: Credentials -> String -> IO ()
chatbot c sysPrompt = go initialHistory where
  go history = postMessages c (req history) >>= \case
    Right resp -> do
      putStrLn resp
      putStrLn sep
      query <- getQuery
      putStrLn sep
      go (history ++ [Message { role = Assistant, content = resp }, Message { role = User, content = query }])
    Left err -> die err

  req history = MessagesRequest { maxTokens = 5000, messages = history, model = defaultModel, system = sysPrompt }

  initialHistory = [Message { role = Assistant, content = "" }]

getQuery :: IO String
getQuery = go [] where
  go ls = do
    l <- getLine
    if l == "."
      then pure (unlines (reverse ls))
      else go (l:ls)

systemPrompt :: [GCal.Event] -> UTCTime -> String
systemPrompt events now = unlines prompt where
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
    ]

  showEvent event =
    let
      timespec = case GCal.eEnd event of
        Just end -> " from " ++ GCal.eStart event ++ " to " ++ end
        Nothing -> " all day event on " ++ GCal.eStart event
      locspec = case GCal.eLocation event of
        Just loc -> " at " ++ loc
        Nothing -> ""
    in "- " ++ GCal.eTitle event ++ timespec ++ locspec

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
