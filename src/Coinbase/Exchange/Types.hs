{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Coinbase.Exchange.Types
  ( ApiType(..)
  , Endpoint
  , Path
  , Pagination(..)
  , nullPagination
  , website
  , sandboxRest
  , sandboxSocket
  , liveRest
  , liveSocket
  , liveRealCoinbaseRest
  , sandboxRealCoinbaseRest
  , Key
  , Secret
  , Passphrase
  , Token
  , key
  , secret
  , passphrase
  , mkToken
  , ExchangeConf(..)
  , ExchangeFailure(..)
  , Exchange
  , ExchangeT
  , runExchange
  , runExchangeT
  , execExchange
  , execExchangeT
  , getManager
  ) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import           Data.Data
import           Data.Text              (Text)
import           GHC.Generics
import           Network.HTTP.Conduit
import           UnliftIO
import           UnliftIO.Resource

-- API URLs
data ApiType
  = Sandbox
  | Live
  deriving (Eq, Show)

type Endpoint = String

type Path = String

website :: Endpoint
website = "https://public.sandbox.gdax.com"

sandboxRest :: Endpoint
sandboxRest = "https://api-public.sandbox.gdax.com"

sandboxSocket :: Endpoint
sandboxSocket = "ws-feed-public.sandbox.gdax.com"

liveRest :: Endpoint
liveRest = "https://api.gdax.com"

liveSocket :: Endpoint
liveSocket = "ws-feed.gdax.com"

-- Coinbase needs to provide real BTC transfers through the exchange API soon,
-- making 2 API calls with 2 sets of authentication credentials is ridiculous.
liveRealCoinbaseRest :: Endpoint
liveRealCoinbaseRest = "https://api.coinbase.com"

sandboxRealCoinbaseRest :: Endpoint
sandboxRealCoinbaseRest = "https://api.sandbox.coinbase.com"

-- Monad Stack
type Key = ByteString

type Secret = ByteString

type Passphrase = ByteString

data Pagination = Pagination
  { before :: Maybe String
  , after  :: Maybe String
  } deriving (Eq, Show)

nullPagination :: Pagination
nullPagination = Pagination {before = Nothing, after = Nothing}

data Token = Token
  { key        :: ByteString
  , secret     :: ByteString
  , passphrase :: ByteString
  } deriving (Eq, Show)

mkToken :: Key -> Secret -> Passphrase -> Either String Token
mkToken k s p =
  case Base64.decode s of
    Right s' -> Right $ Token k s' p
    Left e   -> Left e

data ExchangeConf = ExchangeConf
  { manager   :: Manager
  , authToken :: Maybe Token
  , apiType   :: ApiType
  }

instance Eq ExchangeConf where
  (==) conf1 conf2 =
    (authToken conf1 == authToken conf2) && (apiType conf1 == apiType conf2)

instance Show ExchangeConf where
  show ExchangeConf {..} =
    "ExchangeConf { authToken = " ++
    show authToken ++ ", apiType = " ++ show apiType ++ " }"

data ExchangeFailure
  = ParseFailure Text
  | ApiFailure Text
  | AuthenticationRequiredFailure Text
  | AuthenticationRequiresByteStrings
  deriving (Show, Data, Typeable, Generic)

instance Exception ExchangeFailure

type Exchange a = ExchangeT IO a

newtype ExchangeT m a = ExchangeT
  { unExchangeT :: ResourceT (ReaderT ExchangeConf m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadReader ExchangeConf
             )

deriving instance
         (Monad m, MonadThrow m, MonadIO m, MonadUnliftIO m) =>
         MonadResource (ExchangeT m)

runExchange :: ExchangeConf -> Exchange a -> IO a
runExchange = runExchangeT

runExchangeT :: MonadUnliftIO m => ExchangeConf -> ExchangeT m a -> m a
runExchangeT conf = flip runReaderT conf . runResourceT . unExchangeT

execExchange :: ExchangeConf -> Exchange a -> IO a
execExchange = execExchangeT

execExchangeT :: (MonadUnliftIO m) => ExchangeConf -> ExchangeT m a -> m a
execExchangeT conf act = do
  flip runReaderT conf . runResourceT . unExchangeT $ act

-- Utils
getManager :: (MonadReader ExchangeConf m) => m Manager
getManager = do
  conf <- ask
  return $ manager conf
