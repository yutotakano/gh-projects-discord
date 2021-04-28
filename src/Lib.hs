{-# LANGUAGE RecordWildCards #-}

module Lib
    ( main
    ) where

import           Control.Monad 
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types.Status as Status
import           Web.Scotty

-- | Entry point.
main :: IO ()
main = scotty 3000 $
    post "/github" $ do
        webhook <- decodeWebhook
        liftIO $ putStrLn $ "Received webhook."
        liftIO $ print webhook
        html $ "Hello"

-- | ADT for the Webhook as a whole.
data Webhook = Webhook Context Action Item deriving Show

-- | ADT for the value inside the webhook.
data Item = Project Value
          | ProjectCard Value
          | ProjectColumn Value
          deriving Show

-- | ADT for the context in which the action was performed in.
data Context = Context  { senderName    :: String
                        , senderImage   :: String
                        , repoName      :: String
                        , repoOwnerName :: String
                        }
                        deriving Show

-- | ADT for the action verb that was performed on an item.
data Action = Created
            | Edited
            | Moved
            | Converted
            | Closed
            | Reopened
            | Deleted
            deriving Show

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

-- | Parses a decoded JSON Value to a Webhook.
parseWebhook :: Value -> Parser Webhook
parseWebhook v = do
    context <- parseContext v
    action <- parseAction v
    item <- parseItem v
    pure $ Webhook context action item

-- | Parses a decoded JSON Value to a Context.
parseContext :: Value -> Parser Context
parseContext = withObject "entire webhook" $ \o -> do
    sender <- o .: "sender"
    senderName <- sender .: "login"
    senderImage <- sender .: "avatar_url"
    repository <- o .: "repository"
    repoName <- repository .: "name"
    repoOwner <- repository .: "owner"
    repoOwnerName <- repoOwner .: "login"
    pure $ Context senderName senderImage repoName repoOwnerName

-- | Parses a decoded JSON Value to an Action.
parseAction :: Value -> Parser Action
parseAction = withObject "entire webhook" $ \o -> do
    action <- o .: "action" :: Parser String
    case action of
        "created"   -> pure Created
        "edited"    -> pure Edited
        "moved"     -> pure Moved
        "converted" -> pure Converted
        "closed"    -> pure Closed
        "reopened"  -> pure Reopened
        "deleted"   -> pure Deleted
        _           -> fail "Action not supported."

-- | Parses a decoded JSON Value to an Item.
parseItem :: Value -> Parser Item
parseItem = withObject "entire webhook" $ \o ->
    asum [ ProjectCard <$> (o .: "project_card")
         , ProjectColumn <$> (o .: "project_column")
         , Project <$> (o .: "project")
         ]

-- | End request with a 400.
badReq :: TL.Text -> ActionM a
badReq t = do
    status Status.badRequest400
    text t
    finish
