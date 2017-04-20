{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Prime.Client.Monad
    ( App, runApp
    , asks

    , -- * Config
      Config
    , mkConfig
    , shareSafeAddress
    , shareSafePort
    , shareSafeBaseUrl, Scheme(..)
    , shareSafePath

    , -- * State
      State
    , withState

    , -- * Client query
      ClientM
    , runQuery
    ) where

import Prime.Common.Base

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (ReaderT, MonadReader, asks, runReaderT)
import Control.Monad.Catch (MonadThrow)

import Control.Concurrent (MVar, newMVar, modifyMVar)

import Prime.Secret (MonadRandom(..))

import Servant (ServantErr, AuthProtect)
import Servant.Client ( ClientEnv(..), BaseUrl(..), Scheme(..)
                      , ClientM, runClientM
                      , AuthenticateReq
                      )
import Network.HTTP.Client (newManager, defaultManagerSettings)

newtype App a = App
  { runApp_ :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad
             , MonadError ServantErr
             , MonadIO
             , MonadThrow
             , MonadReader Config
             )
instance MonadRandom App where
    getRandomBytes = liftIO . getRandomBytes

-- | This will be useful for converting the App to a servent server.
--
runApp :: Config -> App a -> ExceptT ServantErr IO a
runApp cfg app = flip runReaderT cfg . runApp_ $ do
    initialise
    app

-- | State Shared between the different handler
--
-- TODO: when we will need to:
--
-- * add here multiple user support
data State = State
    { getClientEnv    :: !ClientEnv
        -- ^ client environment for `servant-client` functions,
        -- calling `sharesafe-lib`'s client functions
    , getSession :: !(Maybe (AuthenticateReq (AuthProtect "cookie-auth")))
    }
  deriving (Typeable)

-- | run one of the ShareSafe client query
--
runQuery :: ClientM a -> App a
runQuery query = do
    env <- withState $ \st -> (st, getClientEnv st)
    r <- liftIO $ runClientM query env
    case r of
        Left _   -> undefined
        Right r' -> return r'

-- | perform some get/set on the State.
--
-- This function is blocking if the state is already in use.
withState :: (State -> (State, a)) -> App a
withState f = do
    mvar <- asks getState
    liftIO $ modifyMVar mvar (return . f)

-- | Configuration passed to the different handler
data Config = Config
    { getState         :: !(MVar State) -- ^ INTERNAL only, use mkConfig
    , shareSafeAddress :: !LString
        -- ^ the address of the sharesafe-server
        --
        -- by default *primetype.co.uk*
    , shareSafePort    :: !Int
        -- ^ the port number of the sharesafe-server
        --
        -- by default *9473*
    , shareSafeBaseUrl :: !Scheme
        -- ^ the scheme either use Http or Https
        --
        -- By default: Https
    , shareSafePath    :: !LString
        -- ^ the base path to use if needed
        --
        -- by default it is empty
        --
        -- TODO: add versioning of the api "api/1"
    }
  deriving (Typeable)

-- | Create the default configuration
--
mkConfig :: MonadIO io => io Config
mkConfig = liftIO $ do
    manager <- liftIO $ newManager defaultManagerSettings
    let cl = ClientEnv manager $ BaseUrl
                defaultShareSafeBaseUrl
                defaultShareSafeAddress
                defaultShareSafePort
                defaultShareSafePath
    st <- newMVar $ State { getClientEnv = cl, getSession = Nothing }
    return $ Config
      { getState         = st
      , shareSafeAddress = defaultShareSafeAddress
      , shareSafePort    = defaultShareSafePort
      , shareSafeBaseUrl = defaultShareSafeBaseUrl
      , shareSafePath    = defaultShareSafePath
      }
  where
    defaultShareSafeAddress = "primetype.co.uk"
    defaultShareSafePort    = 9473
    defaultShareSafeBaseUrl = Https
    defaultShareSafePath    = ""

-- | perform the needed initialisation of the state
--
initialise :: App ()
initialise = do
    addr   <- asks shareSafeAddress
    port   <- asks shareSafePort
    scheme <- asks shareSafeBaseUrl
    path   <- asks shareSafePath
    let burl = BaseUrl scheme addr port path
    withState $ \st ->
        let ClientEnv m _ = getClientEnv st
         in ( st { getClientEnv = ClientEnv m burl }
            , ()
            )
