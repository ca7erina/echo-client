{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Lib
     ( startApp
     )where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client


-- import Network.Wai
-- import Network.Wai.Handler.Warp



newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage


type API = "echo" :> QueryParam "msg" String :> Get '[JSON] HelloMessage

startApp :: IO ()
startApp = run

api :: Proxy API
api = Proxy

echo :: Maybe String -- ^ an optional value for "name"
      -> ClientM HelloMessage
echo = client api


queries :: ClientM (HelloMessage)
queries = do
  message <- echo (Just "test")
  return (message)

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (message) -> do
      print message
