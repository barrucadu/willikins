{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<**>), optional)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
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
  | DailyBriefing RespondArgs
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
    , p "daily-briefing" (DailyBriefing <$> prespond) "Generate the daily briefing"
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
    <$> O.strOption (O.long "chat-id" <> O.metavar "ID" <> O.help "Unique identifier of the conversation to associate this message with")

  p cmd next = O.command cmd . O.info next . O.progDesc

main :: IO ()
main = Mem.withDatabase "willikins.sqlite" $ \db -> do
  credentials <- LLM.credentialsFromEnvironment
  sysPrompt <- LLM.defaultSystemPrompt db
  let llm = LLM.defaultLLM credentials db sysPrompt
  O.execParser parseArgs >>= \case
    Debug DDumpSystemPrompt -> putStrLn sysPrompt
    Debug DDumpFacts -> traverse_ print =<< Mem.getAllFacts db
    Debug DDumpChats -> traverse_ print =<< Mem.getAllChats db
    Sync SCalendar -> syncCalendar db
    DailyBriefing RespondArgs{..} -> respond db rChatId (Just dailyBriefing) llm
    Respond RespondArgs{..} -> respond db rChatId Nothing llm
    Chatbot -> chatbot llm

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
respond :: Mem.Connection -> String -> Maybe String -> LLM.LLM -> IO ()
respond db chatId userPrompt llm = do
    history0 <- Mem.getChatHistory db chatId 50
    prompt <- maybe getContents pure userPrompt
    let (history, humanMessage) = toHistory history0 prompt
    LLM.oneshot llm history >>= \case
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

chatbot :: LLM.LLM -> IO ()
chatbot llm = go initialHistory where
  go history = LLM.oneshot llm history >>= \case
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

dailyBriefing :: String
dailyBriefing = unlines
  [ "It is your duty to provide a briefing summarising important information for the day."
  , "The briefing should have the following sections:"
  , ""
  , "Begin with a formal greeting."
  , ""
  , "**Today**"
  , ""
  , "Note any reminders about today's affairs."
  , ""
  , "**Looking Ahead**"
  , ""
  , "Offer a brief overview of forthcoming appointments, engagements, and tasks for the remainder of the week."
  , "Pay particular attention to tomorrow's schedule."
  , "You do not need to mention when I will be working from home, but do mention if I will be working from the office, or not working at all (apart from weekends when it is expected to not work)"
  , "If I am going to be on call, mention that a week in advance."
  ]

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
