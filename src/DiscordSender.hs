{-# LANGUAGE RecordWildCards #-}

module DiscordSender
    ( sendEmbed
    , Embed(..)
    , EmbedAuthor(..)
    ) where


import           Control.Monad                  ( unless
                                                )
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

-- | ADT for the Discord Webhook Execute request body.
data Params = Params
    { content    :: T.Text
    , username   :: T.Text
    , avatar_url :: T.Text
    , tts        :: Bool
    , embeds     :: [Embed]
    }

instance ToJSON Params where
    toJSON Params{..} = object
        [ "content" .= content
        , "username" .= username
        , "avatar_url" .= avatar_url
        , "tts" .= tts
        , "embeds" .= embeds
        ]

-- | ADT for the Discord Embed type. Unused fields are not implemented.
data Embed = Embed
    { embedTitle :: T.Text
    , embedType :: T.Text
    , embedDescription :: T.Text
    , embedUrl :: T.Text
    , embedColor :: Integer
    , embedAuthor :: EmbedAuthor
    } 

instance ToJSON Embed where
    toJSON Embed{..} = object
        [ "title" .= embedTitle
        , "type" .= embedType
        , "description" .= embedDescription
        , "url" .= embedUrl
        , "color" .= embedColor
        , "author" .= embedAuthor
        ]

-- | ADT for the Discord Embed Author type.
data EmbedAuthor = EmbedAuthor
    { embedAuthorName :: T.Text
    , embedAuthorUrl :: T.Text
    , embedAuthorIcon :: T.Text
    }

instance ToJSON EmbedAuthor where
    toJSON EmbedAuthor{..} = object
        [ "name" .= embedAuthorName
        , "url" .= embedAuthorUrl
        , "icon_url" .= embedAuthorIcon
        ]

-- | Send a POST request to a webhook endpoint, with the Discord params.
sendRequest :: String -> Params -> IO ()
sendRequest endpoint params = do
    initRequest <- parseRequest endpoint
    let request = initRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode params
            , requestHeaders = [("Content-Type", "application/json")]
            }
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    unless (responseStatus res == status204) $
        LBS.putStr $ responseBody res

-- | Wrapper for sendRequest, specifically for sending embeds.
sendEmbed :: String -> Embed -> IO ()
sendEmbed endpoint embed = do
    let params = Params
            { content = ""
            , username = "GitHub"
            , avatar_url = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png"
            , tts = False
            , embeds = [embed]
            }
    sendRequest endpoint params
