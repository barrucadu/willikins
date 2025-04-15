{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO (getLine, putStrLn)
import System.Exit (exitFailure)

import Willikins.LLM.Client

main :: IO ()
main = credentialsFromEnvironment >>= \c -> chatbot c [Message { role = Assistant, content = "" }]

chatbot :: Credentials -> [Message] -> IO ()
chatbot c history = postMessages c req >>= \case
    Right resp -> do
      putStrLn resp
      putStrLn sep
      query <- getQuery
      putStrLn sep
      chatbot c (history ++ [Message { role = Assistant, content = resp }, Message { role = User, content = query }])
    Left err -> die err
  where
    req = MessagesRequest { maxTokens = 5000, messages = history, model = defaultModel, system = systemPrompt }

getQuery :: IO String
getQuery = go [] where
  go ls = do
    l <- getLine
    if l == "."
      then pure (unlines (reverse ls))
      else go (l:ls)

systemPrompt :: String
systemPrompt = unlines
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
  ]

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
