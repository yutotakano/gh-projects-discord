{-# LANGUAGE RecordWildCards #-}

module Lib
    ( main
    ) where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.Aeson
import           Data.Aeson.Types           ( parseEither )
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types.Status as Status
import           Web.Scotty

import           GitHubWebhookDecoder

-- | Entry point.
main :: IO ()
main = scotty 3000 $
    post "/github" $ do
        webhook <- decodeWebhook
        liftIO $ putStrLn $ "Received webhook."
        liftIO $ print webhook
        html $ "Hello"

-- | Decode the JSON in the request body to a Webhook.
-- Throws 400 if the body is empty or unparsable.
decodeWebhook :: ActionM Webhook
decodeWebhook = do
    body <- body
    decoded <- case eitherDecode body of
        Left x  -> badReq (TL.pack x)
        Right x -> pure x

    let webhook = parseEither parseWebhook decoded
    case webhook of
        Left x  -> badReq (TL.pack x)
        Right x -> pure x

-- | End request with a 400.
badReq :: TL.Text -> ActionM a
badReq t = do
    status Status.badRequest400
    text t
    finish
