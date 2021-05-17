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

import           DiscordSender
import           GitHubWebhookDecoder
import           GitHubFurtherRequester

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
-- Silently fails if the body is empty or unparsable.
decodeWebhook :: ActionM Webhook
decodeWebhook = do
    body <- body
    decoded <- case eitherDecode body of
        Left x  -> silentFail (TL.pack x)
        Right x -> pure x

    let webhook = parseEither parseWebhook decoded
    case webhook of
        Left x  -> silentFail (TL.pack x)
        Right x -> pure x

-- | End request with a 202. Used to show success, but nothing acted.
silentFail :: TL.Text -> ActionM a
silentFail t = do
    status Status.status202
    text t
    finish

-- | Create an Embed in the IO Monad, using the GH token
-- and the Webhook data.
createEmbed :: Token -> Webhook -> IO Embed
createEmbed t (Webhook c a (Project v)) = do
    let boardName = parseMaybeDef parseBoardName "Untitled Board" v
    let boardUrl = parseMaybeDef parseBoardUrl "" v
    let boardBody = parseMaybeDef parseBoardBody "(empty)" v
    let title = titlePrefix c <> "Project Board " <> (T.pack $ show a) <> ": " <> boardName
    let description = "Note: " <> boardBody <> ""
    pure $ (def c a)
        { embedTitle = title
        , embedDescription = description
        , embedUrl = boardUrl
        }

createEmbed t (Webhook c a (ProjectColumn v)) = do
    let columnName = parseMaybeDef parseColumnName "" v
    (boardName, boardUrl) <- getBoard t v
    let title = titlePrefix c <> "Project Column " <> (T.pack $ show a) <> ": " <> boardName
    let description = "Column " <> (T.pack $ show a) <> ": **" <> columnName <> "**"
    pure $ (def c a)
        { embedTitle = title
        , embedDescription = description
        , embedUrl = boardUrl
        }

createEmbed t (Webhook c Moved (ProjectCard v)) = do
    -- Handle move event differently
    cardNote <- getCardNote t v

    let columnApiUrl = parseMaybeDef parseCardColumnUrl "" v
    column <- requestFurther t columnApiUrl
    let columnName = parseMaybeDef parseColumnName "" column
    (boardName, boardUrl) <- getBoard t v
    let title = titlePrefix c <> "Project Card Moved: " <> boardName
    let description = "Moved card " <> cardNote <> ".\n\nNew Column: **" <> columnName <> "**"
    pure $ (def c Moved)
        { embedTitle = title
        , embedDescription = description
        , embedUrl = boardUrl
        }

createEmbed t (Webhook c a (ProjectCard v)) = do
    cardNote <- getCardNote t v
    (boardName, boardUrl) <- getBoard t v
    let title = titlePrefix c <> "Project Card " <> (T.pack $ show a) <> ": " <> boardName
    let description = "Card " <> (T.pack $ show a) <> ": " <> cardNote
    pure $ (def c a)
        { embedTitle = title
        , embedDescription = description
        , embedUrl = boardUrl
        }

-- | Create a "[repo/user]" prefix for all embed titles.
titlePrefix :: Context -> T.Text
titlePrefix c = "[" <> repoOwnerName c <> "/" <> repoName c <> "] "

-- | Run a parser with a default value if it fails.
parseMaybeDef :: (a -> Parser b) -> b -> a -> b
parseMaybeDef parser def v  =
    case parseMaybe parser v of
        Nothing -> def
        Just x  -> x

-- | Request GH for a board object through the preview GitHub Project API.
getBoard :: Token -> Value -> IO (T.Text, T.Text)
getBoard token v = do
    let projectApiUrl = parseMaybeDef parseProjectUrl "" v
    projectBoard <- requestFurther token projectApiUrl
    let projectBoardName = parseMaybeDef parseBoardName "Untitled Board" projectBoard
    let projectBoardUrl = parseMaybeDef parseBoardUrl "" projectBoard

    pure (projectBoardName, projectBoardUrl)

-- | Parse the card note, or if it's an Issue, request GH for an issue object
-- and get the title and link.
getCardNote :: Token -> Value -> IO T.Text
getCardNote t v = case parseMaybe parseCardNote v of
    Just x  -> pure $ "**" <> x <> "**"
    Nothing -> do
        -- Attempt to grab the relevant issue
        let mbIssueApiUrl = parseMaybe parseCardIssueUrl v
        case mbIssueApiUrl of
            Nothing -> pure "(empty)"
            Just issueApiUrl  -> do
                issue <- requestFurther t issueApiUrl
                let issueTitle = parseMaybeDef parseIssueTitle "" issue
                let issueUrl = parseMaybeDef parseIssueUrl "" issue
                pure $ "[**" <> issueTitle <> "**](" <> issueUrl <> ")"

-- | Default ADT for Embed.
def :: Context -> Action -> Embed
def c a = Embed { embedTitle = ""
                , embedUrl = ""
                , embedDescription = ""
                , embedType = "rich"
                , embedColor = colorFromAction a
                , embedAuthor = createAuthor c
                }

-- | Transform Actions to decimal color codes.
colorFromAction :: Action -> Integer
colorFromAction Created = 6667344 -- #65bc50
colorFromAction Edited = 16766784 -- #ffd740
colorFromAction Moved = 16766784 
colorFromAction Closed = 0
colorFromAction Reopened = 11771355 -- #b39ddb
colorFromAction Deleted = 6588634 -- #6488da

-- | Transform Context to an EmbedAuthor for Embeds.
createAuthor :: Context -> EmbedAuthor
createAuthor context = EmbedAuthor { embedAuthorName = senderName context
                                   , embedAuthorUrl = "https://github.com/" <> senderName context
                                   , embedAuthorIcon = senderImage context
                                   }
