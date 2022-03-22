{-# LANGUAGE TypeApplications #-}

module App.Env (
  Env (..),
  Config (..),
  initialize,
  Stage (..),
) where

import qualified Configuration.Dotenv as Dotenv
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Logger as MonadLogger
import Data.Aeson
import qualified Data.Char as Char
import Data.Pool (Pool)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word8)
import qualified Database.Esqueleto.Experimental as Esq
import qualified Database.Persist.Postgresql as PersistPG
import Flow ((<|), (|>))
import GHC.Generics (Generic)
import qualified Network.Wai as Wai
import qualified System.Envy as Envy
import System.IO as IO
import qualified Text.Read as Text

data Env = Env
  { dbPool :: Pool Esq.SqlBackend
  , config :: Config
  }

data Stage
  = Development
  | Production
  deriving
    ( Eq
    , Generic
    )
instance Show Stage where
  show Development = "Development"
  show Production = "Production"

instance Envy.Var Stage where
  toVar = show
  fromVar (fmap Char.toLower -> "development") = Just Development
  fromVar (fmap Char.toLower -> "production") = Just Production
  fromVar _ = Nothing

data Config = Config
  { databaseUrl :: Text
  , maxDatabaseConnections :: Word8
  , port :: Word16
  , stage :: Stage
  , secretKey :: Text
  }
  deriving (Generic, Show)

instance Envy.FromEnv Config

-------------------------------------------------------------------------------
type Init a = (ExceptT String IO) a

createConnectionsPool :: Config -> Init (Pool Esq.SqlBackend)
createConnectionsPool config =
  PersistPG.createPostgresqlPool connUrl maxConns
    |> MonadLogger.runStdoutLoggingT
    |> liftIO
 where
  connUrl =
    cs . databaseUrl $ config
  maxConns =
    fromIntegral <| maxDatabaseConnections config

initialize :: Init Env
initialize = do
  config <- ExceptT $ do
    _ <-
      Dotenv.loadFile Dotenv.defaultConfig
        `Dotenv.onMissingFile` pure []
    Envy.decodeEnv @Config

  dbPool <- createConnectionsPool config
  return $
    Env
      { dbPool = dbPool
      , config = config
      }
