module App.Monad (
  AppM (..),
  MonadApp,
  toServantHandler,
) where

import App.Env (Env)
import Control.Monad ((<=<))
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as MonadError
import qualified Control.Monad.Except as MonadExcept
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as MonadIO
import Control.Monad.Logger (LoggingT, MonadLogger (..))
import qualified Control.Monad.Logger as MonadLogger
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as MonadReader
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Servant

newtype AppM a = AppM
  { runAppM :: ExceptT Servant.ServerError (ReaderT Env (LoggingT IO)) a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadReader Env
    , MonadError Servant.ServerError
    , MonadFail
    , MonadIO
    , MonadBase IO
    , MonadBaseControl IO
    , MonadLogger
    )

-- | constraints for performing all operations in our `AppM` monad transformer stack
type MonadApp m =
  ( Monad m
  , MonadReader Env m
  , MonadError Servant.ServerError m
  , MonadFail m
  , MonadIO m
  , MonadBase IO m
  , MonadBaseControl IO m
  , MonadLogger m
  )

toServantHandler :: Env -> AppM a -> Servant.Handler a
toServantHandler context =
  MonadExcept.liftEither
    <=< MonadIO.liftIO
      . MonadLogger.runStdoutLoggingT
      . flip MonadReader.runReaderT context
      . MonadExcept.runExceptT
      . runAppM
