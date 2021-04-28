module GitHubWebhookDecoder
    ( Webhook(..)
    , parseWebhook
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable          ( asum )
import qualified Data.HashMap.Strict as HM

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

