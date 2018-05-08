{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( runServer
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import Servant.API
import Servant.Server

type MyAPI = 
  "api" :> "ping" :> Get '[JSON] String
  :<|> "api" :> "hook" :> ReqBody '[JSON] () :> Post '[JSON] ()

pingHandler :: Handler String
pingHandler = do
  liftIO $ print "Received a ping!"
  return "Hello World!!!!??"

hookHandler :: () -> Handler ()
hookHandler _ = do
  liftIO $ print "Received a hook!"
  return ()

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

myServer :: Server MyAPI
myServer = pingHandler :<|> hookHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve myAPI myServer)
