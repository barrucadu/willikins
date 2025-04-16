{-# LANGUAGE OverloadedStrings #-}

module Willikins.Integration.GoogleCalendar
  ( Credentials
  , credentialsFromEnvironment
  , CalendarId(..)
  , Event(..)
  , fetchEvents
  ) where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.String (fromString)
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import qualified Data.Time.Format as TF
import qualified Network.HTTP.Simple as H
import System.Environment (getEnv)
import qualified System.Process as P

data Credentials = Credentials
  { crCalendarId :: String
  , crCredentialsFile :: String
  }

-- | Read the "GOOGLE_CALENDAR_ID" and "GOOGLE_CREDENTIALS_FILE"
-- environment variables
credentialsFromEnvironment :: IO Credentials
credentialsFromEnvironment = Credentials
  <$> getEnv "GOOGLE_CALENDAR_ID"
  <*> getEnv "GOOGLE_CREDENTIALS_FILE"

-------------------------------------------------------------------------------

newtype CalendarId = CalendarId String
  deriving Show

newtype EventsListResponse = EventsListResponse
  { elrItems :: [Event]
  }

instance FromJSON EventsListResponse where
  parseJSON = A.withObject "EventList" $ \v -> EventsListResponse <$> v A..: "items"

data Event = Event
  { eTitle :: String
  , eLocation :: Maybe String
  , eStart :: String
  , eEnd :: String
  , eAllDay :: Bool
  }
  deriving Show

instance FromJSON Event where
  parseJSON = A.withObject "Event" $ \v -> do
    startv <- v A..: "start"
    endv <- v A..: "end"

    title <- v A..: "summary"
    loc <- v A..:? "location"

    allDayEvent <- startv A..:? "date"
    case allDayEvent of
      Just date -> do
        end <- endv A..: "date"
        pure Event { eTitle = title, eLocation = loc, eStart = date, eEnd = end, eAllDay = True }
      Nothing -> do
        start <- startv A..: "dateTime"
        end <- endv A..: "dateTime"
        pure Event { eTitle = title, eLocation = loc, eStart = start, eEnd = end, eAllDay = False }

fetchEvents :: Credentials -> IO [Event]
fetchEvents credentials = do
    accessToken <- googleAuthAccessToken credentials
    now <- getCurrentTime
    let req = H.setRequestBearerAuth accessToken . H.setRequestQueryString (query now) $ H.parseRequest_ ("GET " ++ url)
    elrItems . H.getResponseBody <$> H.httpJSON req
  where
    url = "https://www.googleapis.com/calendar/v3/calendars/" ++ crCalendarId credentials ++ "/events"

    query now =
      [ ("maxResults", Just "1000")
      , ("orderBy", Just "startTime")
      , ("singleEvents", Just "true")
      , ("timeMin", Just (timeMin now))
      , ("timeMax", Just (timeMax now))
      ]

    timeMin now = fmt $ addUTCTime (negate $ nominalDay * 30) now
    timeMax now = fmt $ addUTCTime (nominalDay * 30) now
    fmt = fromString . TF.formatTime TF.defaultTimeLocale "%Y-%m-%dT00:00:00Z"

-------------------------------------------------------------------------------

googleAuthAccessToken :: Credentials -> IO BS.ByteString
googleAuthAccessToken credentials = do
  P.callProcess "gcloud" ["auth", "activate-service-account", "--key-file=" ++ crCredentialsFile credentials]
  -- the `--help` doesn't say that there's a `--scopes` parameter, but there is,
  -- and it's required in this case
  stdout <- P.readProcess "gcloud" ["auth", "print-access-token", "--scopes=https://www.googleapis.com/auth/calendar.events.readonly"] ""
  let token = head $ lines stdout
  pure (fromString token)
