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
        parsed <- decodeBody
        liftIO $ putStrLn $ "Received webhook."
        liftIO $ print parsed
        html $ "Hello"

-- | Decode the JSON in the request body.
-- Throws 400 if the body is empty or unparsable.
decodeBody :: ActionM Webhook
decodeBody = do
    body <- body
    decoded <- case eitherDecode body of
        Left x  -> badReq (TL.pack x)
        Right x -> pure x
    let webhook = parseEither parseWebhook decoded
    case webhook of
        Left x  -> badReq (TL.pack x)
        Right x -> pure x

data Action = Created
            | Edited
            | Moved
            | Converted
            | Closed
            | Reopened
            | Deleted
            deriving Show

data Webhook = Project Action Value
             | ProjectCard Action Value
             | ProjectColumn Action Value
             deriving Show

data Context = Context  { senderName    :: String
                        , senderImage   :: String
                        , repoName      :: String
                        , repoOwnerName :: String
                        }

parseWebhook :: Value -> Parser Webhook
parseWebhook v = do
    action <- parseAction v
    context <- parseContext v
    parseItem action v

parseItem :: Action -> Value -> Parser Webhook
parseItem action = withObject "entire webhook" $ \o ->
    asum [ ProjectCard action <$> (o .: "project_card")
         , ProjectColumn action <$> (o .: "project_column")
         , Project action <$> (o .: "project")
         ]

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

badReq :: TL.Text -> ActionM a
badReq t = do
    status Status.badRequest400
    text t
    finish
