module Willikins.Utils where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception.Safe as E
import qualified Network.HTTP.Simple as H

-- | HTTP request, decoding the body as JSON, with error handling.
httpRequestJSON :: FromJSON response => H.Request -> IO (Either (Maybe BL.ByteString) response)
httpRequestJSON req = do
  resp <- E.tryAny (H.getResponseBody <$> H.httpLBS req)
  pure $ case resp of
    Right body -> either (const $ Left (Just body)) Right $ A.eitherDecode body
    Left _ -> Left Nothing
