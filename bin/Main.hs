{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<**>), optional)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import qualified Data.Time.Format as TF
import qualified Options.Applicative as O
import qualified System.IO as IO
import System.Exit (exitFailure)

import qualified Willikins.Integration.GoogleCalendar as GCal
import qualified Willikins.LLM as LLM
import qualified Willikins.Memory as Mem

data Args
  = Debug DebugArgs
  | Sync SyncArgs
  | Respond RespondArgs
  | Chatbot

data DebugArgs
  = DDumpSystemPrompt
  | DDumpFacts
  | DDumpChats

data SyncArgs = SCalendar

newtype RespondArgs = RespondArgs { rChatId :: String }

parseArgs :: O.ParserInfo Args
parseArgs = O.info (parser <**> O.helper) (O.fullDesc <> O.header "willikins - your AI butler") where
  parser = fromMaybe Chatbot <$> optional parser'

  parser' = O.hsubparser . mconcat $
    [ p "debug" (Debug <$> pdebug) "Debugging tools"
    , p "sync" (Sync <$> psync) "Pull data from external sources"
    , p "respond" (Respond <$> prespond) "Simple one-shot communication"
    ]

  pdebug = O.hsubparser . mconcat $
    [ p "dump-system-prompt" (pure DDumpSystemPrompt) "Print system prompt"
    , p "dump-facts" (pure DDumpFacts) "Print all remembered facts"
    , p "dump-chats" (pure DDumpChats) "Print all chat history"
    ]

  psync = O.hsubparser . mconcat $
    [ p "calendar" (pure SCalendar) "Synchronise google calendar"
    ]

  prespond = RespondArgs
    <$> O.strOption (O.long "chat-id" <> O.metavar "ID" <> O.help "Unique identifier of the conversation to associate this message and its response with")

  p cmd next = O.command cmd . O.info next . O.progDesc

main :: IO ()
main = Mem.withDatabase "willikins.sqlite" $ \db -> do
  credentials <- LLM.credentialsFromEnvironment
  events <- Mem.getAllEvents db
  facts <- Mem.getAllFacts db
  now <- getCurrentTime
  let sysPrompt = systemPrompt now events facts
  let llm = LLM.defaultLLM credentials db
  O.execParser parseArgs >>= \case
    Debug DDumpSystemPrompt -> putStrLn sysPrompt
    Debug DDumpFacts -> traverse_ print facts
    Debug DDumpChats -> traverse_ print =<< Mem.getAllChats db
    Sync SCalendar -> syncCalendar db
    Respond RespondArgs{..} -> respond db rChatId sysPrompt llm
    Chatbot -> chatbot sysPrompt llm

syncCalendar :: Mem.Connection -> IO ()
syncCalendar db = GCal.credentialsFromEnvironment >>= GCal.fetchEvents >>= \case
  Just events -> do
    Mem.replaceAllEvents db $ map GCal.formatEventForLLM events
    putStrLn $ "loaded " ++ show (length events) ++ " events from Google Calendar"
  Nothing -> do
    putStrLn "could not fetch events from Google Calendar"
    exitFailure

-- | Reply to a single message provided on stdin, printing the result(s) to
-- stdout in JSON format, and also appending to the conversation history in
-- memory.
respond :: Mem.Connection -> String -> String -> LLM.LLM -> IO ()
respond db chatId sysPrompt llm = do
    history0 <- Mem.getChatHistory db chatId 50
    prompt <- getContents
    let (history, humanMessage) = toHistory history0 prompt
    LLM.oneshot llm sysPrompt history >>= \case
      Right history' -> do
        Mem.insertChatMessages db chatId $ maybe id (:) humanMessage history'
        BL.putStr (A.encode history')
        putStrLn ""
      Left err -> do
        IO.hPrint IO.stderr err
        exitFailure
  where
    -- beginning of a new conversation, AI opening
    toHistory [] "" = ([LLM.MessageText { mtRole = LLM.Assistant, mtText = "" }], Nothing)
    -- continuance of an existing conversation, AI turn
    toHistory hs "" = (hs, Nothing)
    -- beginning of a new conversation, human opening
    toHistory [] prompt = let human = LLM.MessageText { mtRole = LLM.User, mtText = prompt } in ([human], Just human)
    -- continuance of an existing conversation, human turn
    toHistory hs prompt = let human = LLM.MessageText { mtRole = LLM.User, mtText = prompt } in (hs ++ [human], Just human)

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

systemPrompt :: UTCTime -> [Mem.Event] -> [Mem.Fact] -> String
systemPrompt now events facts = unlines prompt where
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
