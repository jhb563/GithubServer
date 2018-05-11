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
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import Servant.API
import Servant.Server

type MyAPI = 
  "api" :> "ping" :> Get '[JSON] String
  :<|> "api" :> "hook" :> ReqBody '[JSON] GithubRequestPayload :> Post '[JSON] Text

data GithubRequestPayload = 
  GitOpenPullRequest Text Text |
  GitCommit  Text Text Text |
  GitOtherRequest
  deriving (Show)

instance FromJSON GithubRequestPayload where
  parseJSON = withObject "GithubRequestPlayoad" $ \o -> do
    (action :: Maybe Text) <- o .:? "action"
    prSectionMaybe <- o .:? "pull_request"
    case (action, prSectionMaybe) of
      (Just "opened", Just prSection :: Maybe Value) -> do
        (userSection :: Value, commentsURL :: Text) <- withObject "PR Section" fetchUserAndComments prSection
        userName <- withObject "User Section" (\o' -> o' .: "login") userSection
        return $ GitOpenPullRequest userName commentsURL
      _ -> return GitOtherRequest
    where
      fetchUserAndComments o' = do
        uSection <- o' .: "user"
        commentsURL <- o' .: "review_comments_url"
        return (uSection, commentsURL)

pingHandler :: Handler String
pingHandler = do
  liftIO $ print "Received a ping!"
  return "Hello World!!!!??"

hookHandler :: GithubRequestPayload -> Handler Text
hookHandler GitOtherRequest = return "Found some other request!"
hookHandler (GitOpenPullRequest userName commentsURL) = return $
  "User: " <> userName <> " opened a pull request with comments at: " <> commentsURL

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

myServer :: Server MyAPI
myServer = pingHandler :<|> hookHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve myAPI myServer)
