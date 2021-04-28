module Lib
    ( main
    ) where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.Aeson
import           Data.Aeson.Types           ( parseEither
                                            , parseMaybe
                                            , Parser(..)
                                            )
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as Status
import           System.Environment
import           Web.Scotty

import           GitHubWebhookDecoder
import           DiscordSender

-- | Entry point.
main :: IO ()
main = do
    args <- getArgs
    webhookUrl <- case () of
        _ | length args == 0 -> error "URL not supplied"
        _                    -> pure $ head args
    scotty 3000 $
        post "/github" $ do
            webhook <- decodeWebhook
            liftIO $ sendEmbed webhookUrl $ createEmbed webhook
            liftIO $ print webhook
            html $ "Success."

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

createEmbed :: Webhook -> Embed
createEmbed (Webhook c a (Project v)) = createProjectEmbed c a v
createEmbed (Webhook c a (ProjectColumn v)) = createProjectColumnEmbed c a v
createEmbed (Webhook c a (ProjectCard v)) = createProjectCardEmbed c a v

createProjectEmbed :: Context -> Action -> Value -> Embed
createProjectEmbed c a v = Embed { embedTitle = title
                                 , embedType = "rich"
                                 , embedDescription = ""
                                 , embedUrl = projectBoardUrl
                                 , embedColor = colorFromAction a
                                 }
  where
    title :: T.Text
    title = titlePrefix <> "Project Board " <> (T.pack $ show a) <> ": " <> projectBoardName

    titlePrefix :: T.Text
    titlePrefix = "[" <> repoOwnerName c <> "/" <> repoName c <> "] "

    projectBoardName :: T.Text
    projectBoardName = parseMaybeDef parseBoardName "" v

    projectBoardUrl :: T.Text
    projectBoardUrl = parseMaybeDef parseBoardUrl "" v

createProjectColumnEmbed :: Context -> Action -> Value -> Embed
createProjectColumnEmbed = undefined

createProjectCardEmbed :: Context -> Action -> Value -> Embed
createProjectCardEmbed = undefined

parseMaybeDef :: (a -> Parser b) -> b -> a -> b
parseMaybeDef parser def v  =
    case parseMaybe parser v of
        Nothing -> def
        Just x  -> x

colorFromAction :: Action -> Integer
colorFromAction Created = 6667344 -- #65bc50
colorFromAction Edited = 16766784 -- #ffd740
colorFromAction Moved = 0
colorFromAction Converted = 16766784 -- #ffd740
colorFromAction Closed = 0
colorFromAction Reopened = 11771355 -- #b39ddb
colorFromAction Deleted = 6588634 -- #6488da
 
