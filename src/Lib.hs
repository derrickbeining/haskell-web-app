{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib (
  startApp,
) where

import App.Env (
  Config (..),
  Env (..),
  initialize,
 )
import App.Monad (AppM)
import qualified App.Monad as App
import Control.Monad.Except (runExceptT)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Flow ((<|), (|>))
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServerT, genericServeT)

data User = User
  { userId :: Int
  , userFirstName :: String
  , userLastName :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data Api route = Api
  { health :: route :- Get '[JSON] Text
  , users :: route :- "users" :> Get '[JSON] [User]
  }
  deriving stock
    ( Generic
    )

startApp :: IO ()
startApp = do
  runExceptT App.Env.initialize
    >>= either putStrLn mkWebServer
 where
  mkWebServer env = do
    let port = App.Env.port <| App.Env.config env
    putStrLn $ "Listening on port " <> show port
    Warp.run (fromIntegral port) $ mkApp env

mkApp :: Env -> Application
mkApp env =
  genericServeT (App.toServantHandler env) api

api :: Api (AsServerT AppM)
api =
  Api
    { health = pure "ok"
    , users = pure mockUsers
    }

mockUsers :: [User]
mockUsers =
  [ User 1 "Isaac" "Newton"
  , User 2 "Albert" "Einstein"
  ]
