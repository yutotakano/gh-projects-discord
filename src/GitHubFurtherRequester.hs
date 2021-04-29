module GitHubFurtherRequester
    ( Token
    , requestFurther
    ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

type Token = T.Text

requestFurther :: Token -> T.Text -> IO Value
requestFurther token url = do
    let [user, password] = T.splitOn ":" token
    initReq <- parseRequest $ T.unpack url
    let initReq' = initReq { requestHeaders = [("Accept", "application/vnd.github.inertia-preview+json")
                                              ,("User-Agent", "yutotakano/gh-projects-discord")
                                              ]
                           }
    let req = applyBasicAuth (TE.encodeUtf8 user) (TE.encodeUtf8 password) initReq'
    manager <- newManager tlsManagerSettings
    res <- httpLbs req manager

    case eitherDecode (responseBody res) of
        Left x  -> fail "GitHub API Response Error"
        Right x -> pure x

