{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server
  ( runServer
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text, unpack, pack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import Servant.API
import Servant.Client
import Servant.Server

type MyAPI = 
  "api" :> "ping" :> Get '[JSON] String
  :<|> "api" :> "hook" :> ReqBody '[JSON] GithubRequest :> Post '[JSON] Text

type GithubAPI = BasicAuth "GithubUser" () :> ReqBody '[JSON] GitPRComment :> Post '[JSON] ()

sendCommentClient :: BasicAuthData -> GitPRComment -> ClientM ()
sendCommentClient = client (Proxy :: Proxy GithubAPI)

data GithubRequest = 
  GitOpenPRRequest Text Text |
  GithubOtherRequest
  deriving (Show)

instance FromJSON GithubRequest where
  parseJSON = withObject "GithubRequestPlayoad" $ \o -> do
    (action :: Maybe Text) <- o .:? "action"
    prSectionMaybe <- o .:? "pull_request"
    case (action, prSectionMaybe) of
      (Just "opened", Just prSection :: Maybe Value) -> do
        (userSection :: Value, commentsURL :: Text) <- withObject "PR Section" fetchUserAndComments prSection
        userName <- withObject "User Section" (\o' -> o' .: "login") userSection
        return $ GitOpenPRRequest userName commentsURL
      _ -> return GithubOtherRequest
    where
      fetchUserAndComments o' = do
        uSection <- o' .: "user"
        commentsURL <- o' .: "comments_url"
        return (uSection, commentsURL)

newtype GitPRComment = GitPRComment Text

instance ToJSON GitPRComment where
  toJSON (GitPRComment body) = object [ "body" .= body ]

pingHandler :: Handler String
pingHandler = do
  liftIO $ print "Received a ping!"
  return "Hello World!"

hookHandler :: GithubRequest -> Handler Text
hookHandler GithubOtherRequest = return "Found a non-PR opening request."
hookHandler (GitOpenPRRequest userName commentsURL) = do
  liftIO $ addComment userName commentsURL
  return $ "User: " <> userName <> 
    " opened a pull request with comments at: " <> commentsURL

addComment :: Text -> Text -> IO ()
addComment userName commentsURL = do
  gitUsername <- getEnv "GITHUB_USERNAME"
  gitPassword <- getEnv "GITHUB_PASSWORD"
  let authData = BasicAuthData (BSC.pack gitUsername) (BSC.pack gitPassword)
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl (unpack commentsURL)
  let clientEnv = ClientEnv manager baseUrl
  runClientM (sendCommentClient authData (commentBody gitUsername)) clientEnv
  return ()
  where
    commentBody adminName = GitPRComment $ "Thanks for posting this @" <> userName <>
      "! I'll take a look soon! - @" <> (pack adminName)

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

myServer :: Server MyAPI
myServer = pingHandler :<|> hookHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve myAPI myServer)
