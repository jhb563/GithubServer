{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( runServer
  ) where

import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import Servant.API
import Servant.Server

type MyAPI = "api" :> "ping" :> Get '[JSON] String

pingHandler :: Handler String
pingHandler = return "Hello World!!!!??"

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

myServer :: Server MyAPI
myServer = pingHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve myAPI myServer)
