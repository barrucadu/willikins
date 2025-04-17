{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import qualified Data.Time.Format as TF
import System.IO (getLine, putStrLn)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Willikins.Integration.GoogleCalendar as GCal
import Willikins.LLM.Client

main :: IO ()
main = do
  events <- GCal.fetchEvents =<< GCal.credentialsFromEnvironment
  now <- getCurrentTime
  let sysPrompt = systemPrompt events now
  getArgs >>= \case
    ["debug", "dump-system-prompt"] -> putStrLn sysPrompt
    _ -> credentialsFromEnvironment >>= chatbot sysPrompt

chatbot :: String -> Credentials -> IO ()
chatbot sysPrompt c = go initialHistory where
  go history = postMessages c (req history) >>= \case
    Right resp -> handleResponse history (mrContent resp)
    Left err -> die err

  handleResponse history [] = go history
  handleResponse history (MessagesResponseContentText{..}:ms) = do
    putStrLn mrctText
    putStrLn sep
    query <- getQuery
    putStrLn sep
    let assistant = MessageText { mtRole = Assistant, mtText = mrctText }
    let user = MessageText { mtRole = User, mtText = query }
    handleResponse (history ++ [assistant, user]) ms
  handleResponse history (MessagesResponseContentToolUse{..}:ms) = do
    putStrLn $ "TOOL USE: " ++ mrctuTool
    print mrctuInput
    putStrLn sep
    putStrLn "TOOL RESPONSE:"
    toolResponse <- getToolResponse mrctuTool mrctuInput
    print toolResponse
    putStrLn sep
    let assistant = MessageToolUse { mtuId = mrctuId, mtuTool = mrctuTool, mtuInput = mrctuInput }
    let user = MessageToolResult { mtrId = mrctuId, mtrText = toolResponse }
    handleResponse (history ++ [assistant, user]) ms

  req history = defaultMessagesRequest { messages = history, system = sysPrompt, tools = demoTools }

  initialHistory = [MessageText { mtRole = Assistant, mtText = "" }]

getQuery :: IO String
getQuery = go [] where
  go ls = do
    l <- getLine
    if l == "."
      then pure (unlines (reverse ls))
      else go (l:ls)

getToolResponse :: String -> a -> IO (Either String String)
getToolResponse "create_memory" _ = pure $ Right "New memory ID: MEM123"
getToolResponse name _ = pure $ Left ("No such tool '" ++ name ++ "'")

demoTools :: [Tool]
demoTools = [create_memory] where
  create_memory = Tool
    { tName = "create_memory"
    , tDescription = "Commit a piece of information to memory so that you can recall and reference it later."
    , tArguments =
      [ ToolArgument
        { taName = "text"
        , taType = "string"
        , taDescription = "The information to note down, keep it short but include all important details."
        , taRequired = False
        }
      , ToolArgument
        { taName = "date"
        , taType = "string"
        , taDescription = "The actual date of the event or reminder.  Set a date whenever possible, but if no date is relevant leave this out."
        , taRequired = False
        }
      ]
    }

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
    , ""
    , "Before responding to the user, use the create_memory tool to record any reminders or any other information that might be useful to reference later."
    ]

  showEvent e = "- " ++ GCal.formatEventForLLM e

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
