{-# LANGUAGE LambdaCase #-}
module Willikins.Utils where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import qualified Control.Exception.Safe as E
import qualified Network.HTTP.Simple as H

-- | HTTP request, decoding the body as a lazy bytestring, or `Nothing` on error.
httpRequest :: H.Request -> IO (Maybe BL.ByteString)
httpRequest req = either (const Nothing) Just <$> E.tryAny resp where
  resp = H.getResponseBody <$> H.httpLBS req

-- | HTTP request, decoding the body as JSON, with error handling.
httpRequestJSON :: FromJSON response => H.Request -> IO (Either (Maybe BL.ByteString) response)
httpRequestJSON req = httpRequest req <&> \case
  Just body -> either (const $ Left (Just body)) Right $ A.eitherDecode body
  Nothing -> Left Nothing
