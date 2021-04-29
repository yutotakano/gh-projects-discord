module Lib
    ( main
    ) where

import           Control.Monad.IO.Class     ( liftIO )
import           Data.Aeson
import           Data.Aeson.Types           ( parseEither
                                            , parseMaybe
                                            , Parser(..)
                                            )
import           Data.Char                  ( isSpace )
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Types.Status as Status
import           System.Environment
import           Web.Scotty

import           GitHubWebhookDecoder
import           GitHubFurtherRequester
import           DiscordSender

-- | Entry point.
main :: IO ()
main = do
    args <- getArgs
    webhookUrl <- case () of
        _ | length args == 0 -> error "URL not supplied"
        _                    -> pure $ head args
    tokenContents <- TIO.readFile "gh.token" :: (IO Token)
    let token = T.takeWhile (not . isSpace) tokenContents
    
    scotty 3000 $
        post "/github" $ do
            webhook <- decodeWebhook
            liftIO $ createEmbed token webhook >>= sendEmbed webhookUrl
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

createEmbed :: Token -> Webhook -> IO Embed
createEmbed t (Webhook c a (Project v))       = createProjectEmbed t c a v
createEmbed t (Webhook c a (ProjectColumn v)) = createProjectColumnEmbed t c a v
createEmbed t (Webhook c a (ProjectCard v))   = createProjectCardEmbed t c a v

createProjectEmbed :: Token -> Context -> Action -> Value -> IO Embed
createProjectEmbed t c a v = do
    let projectBoardName = parseMaybeDef parseBoardName "" v
    let projectBoardUrl = parseMaybeDef parseBoardUrl "" v
    let projectBoardBody = parseMaybeDef parseBoardBody "" v
   
    let title = titlePrefix c <> "Project Board " <> (T.pack $ show a) <> ": " <> projectBoardName
    let description = "Description: `" <> projectBoardBody <> "`"
    pure $
        Embed { embedTitle = title
              , embedType = "rich"
              , embedDescription = ""
              , embedUrl = projectBoardUrl
              , embedColor = colorFromAction a
              , embedAuthor = createAuthor c
              }

createProjectColumnEmbed :: Token -> Context -> Action -> Value -> IO Embed
createProjectColumnEmbed t c a v = do
    let projectColumnName = parseMaybeDef parseColumnName "" v
    -- The webhook event does not send the containing Project nor its ID,
    -- so manually request it.
    let projectUrl = parseMaybeDef parseColumnProjectUrl "" v
    projectBoard <- requestFurther t projectUrl

    let projectBoardName = parseMaybeDef parseBoardName "" projectBoard
    let projectBoardUrl = parseMaybeDef parseBoardUrl "" projectBoard
    
    let title = titlePrefix c <> "Project Column " <> (T.pack $ show a) <> ": " <> projectBoardName
    let description = "Column: `" <> projectColumnName <> "`"
    pure $
        Embed { embedTitle = title
              , embedType = "rich"
              , embedDescription = description
              , embedUrl = projectBoardUrl
              , embedColor = colorFromAction a
              , embedAuthor = createAuthor c
              }

createProjectCardEmbed :: Token -> Context -> Action -> Value -> IO Embed
createProjectCardEmbed t c Moved v = do
    -- Handle move event differently
    let cardNote = parseMaybeDef parseCardNote "" v
    let columnUrl = parseMaybeDef parseCardColumnUrl "" v
    column <- requestFurther t columnUrl
    let columnName = parseMaybeDef parseColumnName "" column
    
    let projectUrl = parseMaybeDef parseCardProjectUrl "" v
    projectBoard <- requestFurther t projectUrl
    let projectBoardName = parseMaybeDef parseBoardName "" projectBoard
    let projectBoardUrl = parseMaybeDef parseBoardUrl "" projectBoard

    let title = titlePrefix c <> "Project Card Moved: " <> projectBoardName
    let description = "Moved `" <> cardNote <> "` to `" <> columnName <> "`"
    pure $
        Embed { embedTitle = title
              , embedType = "rich"
              , embedDescription = description
              , embedUrl = projectBoardUrl
              , embedColor = colorFromAction Moved
              , embedAuthor = createAuthor c
              }

createProjectCardEmbed t c a v = do
    let cardNote = parseMaybeDef parseCardNote "" v
    let projectUrl = parseMaybeDef parseCardProjectUrl "" v
    projectBoard <- requestFurther t projectUrl
    let projectBoardName = parseMaybeDef parseBoardName "" projectBoard
    let projectBoardUrl = parseMaybeDef parseBoardUrl "" projectBoard

    let title = titlePrefix c <> "Project Card " <> (T.pack $ show a) <> ": " <> projectBoardName
    let description = "Card: `" <> cardNote <> "`"
    pure $ 
        Embed { embedTitle = title
              , embedType = "rich"
              , embedDescription = description
              , embedUrl = projectBoardUrl
              , embedColor = colorFromAction a
              , embedAuthor = createAuthor c
              }

titlePrefix :: Context -> T.Text
titlePrefix c = "[" <> repoOwnerName c <> "/" <> repoName c <> "] "

parseMaybeDef :: (a -> Parser b) -> b -> a -> b
parseMaybeDef parser def v  =
    case parseMaybe parser v of
        Nothing -> def
        Just x  -> x

colorFromAction :: Action -> Integer
colorFromAction Created = 6667344 -- #65bc50
colorFromAction Edited = 16766784 -- #ffd740
colorFromAction Moved = 16766784 
colorFromAction Closed = 0
colorFromAction Reopened = 11771355 -- #b39ddb
colorFromAction Deleted = 6588634 -- #6488da

createAuthor :: Context -> EmbedAuthor
createAuthor context = EmbedAuthor { embedAuthorName = senderName context
                                   , embedAuthorUrl = "https://github.com/" <> senderName context
                                   , embedAuthorIcon = senderImage context
                                   }
