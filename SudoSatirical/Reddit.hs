{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SudoSatirical.Reddit
( TokenResponse(..)
, SubmitRequest(..)
, RedditError(..)
, getToken
, submitLink
)
where

import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy
import qualified Data.Text as Text
import Data.Text (Text)
import Network.HTTP.Client.TLS
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.Internal (connectionWrite)
import Servant.API hiding (addHeader)
import Servant.API.BasicAuth
import Servant.API.ContentTypes
import Servant.Client
import Servant.Common.Req
import System.IO
import Web.FormUrlEncoded

type API =
    "api" :> "v1" :> "access_token"
        :> BasicAuth "thisisnotarealm" BasicAuthData
        :> ReqBody '[FormUrlEncoded] TokenRequest
        :> Post '[JSON] TokenResponse
    :<|>
    "api" :> "submit"
        :> Header "Authorization" Text
        :> ReqBody '[FormUrlEncoded] SubmitRequest
        :> Post '[JSON] Value

data TokenRequest = TokenRequest
    { trGrantType :: Text
    , trUsername :: Text
    , trPassword :: Text
    }
    deriving (Show, Eq)

instance ToForm TokenRequest where
    toForm (TokenRequest t u p) =
        [ ("grant_type", toQueryParam t)
        , ("username", toQueryParam u)
        , ("password", toQueryParam p)
        ]

data TokenResponse = TokenResponse
    { tsToken :: Text
    , tsExpires :: Int
    , tsScope :: Text
    , tsType :: Text
    }
    deriving (Show, Eq)

instance FromJSON TokenResponse where
    parseJSON = withObject "TokenResponse" $ \v -> TokenResponse
        <$> v .: "access_token"
        <*> v .: "expires_in"
        <*> v .: "scope"
        <*> v .: "token_type"

authString ts = Text.concat [ tsType ts, " ", tsToken ts ]

data SubmitRequest = SubmitRequest
    { srKind :: Text
    , srSubreddit :: Text
    , srTitle :: Text
    , srUrl :: Text
    }
    deriving (Show, Eq)

instance ToForm SubmitRequest where
    toForm (SubmitRequest k s t u) =
        [ ("api_type", toQueryParam ("json" :: Text))
        , ("kind", toQueryParam k)
        , ("sr", toQueryParam s)
        , ("title", toQueryParam t)
        , ("url", toQueryParam u)
        ]

-- { 'json': { 'errors':
--   [ [ 'RATELIMIT', 'you are doing that too much. try again in 9 minutes.'. 'ratelimit' ] ]
--   ..
-- } }

data RedditError = RedditError Text Text
    deriving Show

instance FromJSON RedditError where
    parseJSON = withObject "RedditError" $ \o -> do
        json <- o .: "json"
        [errors] :: [[Text]] <- json .: "errors"

        case errors of
            (t:str:_) -> return $ RedditError t str
            _ -> empty


redditId = "vy3GDWO9LZr3EQ" :: ByteString
redditSecret = "9ObJSVckLBRawouQgEWG5gjnwQ8" :: ByteString
redditUsername = "sudosatiricalbot"
redditPassword = "shogaiquohX2"
redditHost = "reddit.com"
redditOauthHost = "oauth.reddit.com"

userAgent = "sudosatiricalbot (https://reddit.com/r/sudosatirical)"

api = Proxy :: Proxy API

tokenClient :: BasicAuthData -> TokenRequest -> ClientM TokenResponse

submitClient :: Maybe Text -> SubmitRequest -> ClientM Value

tokenClient :<|> submitClient = clientWithRoute api redditReq

redditReq :: Req
redditReq = addHeader "User-Agent" (userAgent :: Text)  defReq

getToken :: IO (Either RedditError TokenResponse)
getToken = do
    let auth = BasicAuthData redditId redditSecret
        client = tokenClient auth (TokenRequest "password" redditUsername redditPassword)

    runClient redditHost client

submitLink :: TokenResponse -> SubmitRequest -> IO (Either RedditError Value)
submitLink ts sr = do
    let auth = authString ts
        client = submitClient (Just auth) sr

    res <- runOauth client
    return $ checkError res

checkError :: Either RedditError Value -> Either RedditError Value
checkError (Left e) = Left e
checkError (Right v) = case fromJSON v of
    Success re -> Left re
    Error _ -> Right v

runClient :: String -> ClientM a -> IO (Either RedditError a)
runClient h c = do
    let settings = tlsManagerSettings { managerModifyRequest = return {- logRequest -} }
    m <- newManager settings
    res <- runClientM c (ClientEnv m (BaseUrl Https h 443 ""))

    return $ case res of
        Left se -> Left $ RedditError "ServantError" (Text.pack . show $ se)
        Right val -> Right val

runOauth = runClient redditOauthHost

logRequest r = do
    hPrint stderr r
    return r
