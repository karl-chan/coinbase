{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Rest
  ( coinbaseGet
  , coinbaseGetPaginated
  , coinbasePost
  , coinbaseDelete
  , coinbaseDeleteDiscardBody
  , coinbaseRequest
  , voidBody
  , processResponse
  , processPaginatedResponse
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Crypto.Hash
import           Data.Aeson
import           Data.Byteable
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as CBS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import           Data.Conduit.Attoparsec      (sinkParser)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Text.Printf
import           UnliftIO

import           Coinbase.Exchange.Types

type Signed = Bool

type IsForExchange = Bool

voidBody :: Maybe ()
voidBody = Nothing

coinbaseGet ::
     (FromJSON b, MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => Signed
  -> Path
  -> m b
coinbaseGet sgn p =
  coinbaseRequest "GET" sgn p voidBody >>= processResponse True

coinbaseGetPaginated ::
     (FromJSON b, MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => Signed
  -> Path
  -> Pagination
  -> m (b, Pagination)
coinbaseGetPaginated sgn p pagination =
  let separator =
        if '?' `elem` p
          then "&"
          else "?"
      suffix =
        case (before pagination, after pagination) of
          (Nothing, Nothing) -> ""
          (Just b, Nothing)  -> "before=" ++ b
          (Nothing, Just a)  -> "after=" ++ a
          (Just b, Just a)   -> "before=" ++ b ++ "&" ++ "after=" ++ a
      paginatedPath = p ++ separator ++ suffix
   in coinbaseRequest "GET" sgn paginatedPath voidBody >>=
      processPaginatedResponse

coinbasePost ::
     ( ToJSON a
     , FromJSON b
     , MonadResource m
     , MonadReader ExchangeConf m
     , MonadThrow m
     )
  => Signed
  -> Path
  -> Maybe a
  -> m b
coinbasePost sgn p ma = coinbaseRequest "POST" sgn p ma >>= processResponse True

coinbaseDelete ::
     ( ToJSON a
     , FromJSON b
     , MonadResource m
     , MonadReader ExchangeConf m
     , MonadThrow m
     )
  => Signed
  -> Path
  -> Maybe a
  -> m b
coinbaseDelete sgn p ma =
  coinbaseRequest "DELETE" sgn p ma >>= processResponse True

coinbaseDeleteDiscardBody ::
     (ToJSON a, MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => Signed
  -> Path
  -> Maybe a
  -> m ()
coinbaseDeleteDiscardBody sgn p ma =
  coinbaseRequest "DELETE" sgn p ma >>= processEmpty

coinbaseRequest ::
     (ToJSON a, MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => Method
  -> Signed
  -> Path
  -> Maybe a
  -> m (Response (ConduitM () BS.ByteString m ()))
coinbaseRequest meth sgn p ma = do
  conf <- ask
  req <-
    case apiType conf of
      Sandbox -> parseUrlThrow $ sandboxRest ++ p
      Live    -> parseUrlThrow $ liveRest ++ p
  let req' =
        req
          { method = meth
          , requestHeaders =
              [("user-agent", "haskell"), ("accept", "application/json")]
          }
  flip http (manager conf) =<<
    signMessage True sgn meth p =<< encodeBody ma req'

encodeBody :: (ToJSON a, Monad m) => Maybe a -> Request -> m Request
encodeBody (Just a) req =
  return
    req
      { requestHeaders =
          requestHeaders req ++ [("content-type", "application/json")]
      , requestBody = RequestBodyBS $ LBS.toStrict $ encode a
      }
encodeBody Nothing req = return req

signMessage ::
     (MonadIO m, MonadReader ExchangeConf m)
  => IsForExchange
  -> Signed
  -> Method
  -> Path
  -> Request
  -> m Request
signMessage isForExchange True meth p req = do
  conf <- ask
  case authToken conf of
    Just tok -> do
      time <-
        liftM (realToFrac . utcTimeToPOSIXSeconds) (liftIO getCurrentTime) >>= \t ->
          return . CBS.pack $ printf "%.0f" (t :: Double)
      rBody <- pullBody $ requestBody req
      let presign = CBS.concat [time, meth, CBS.pack p, rBody]
          sign =
            if isForExchange
              then Base64.encode $
                   toBytes (hmac (secret tok) presign :: HMAC SHA256)
              else digestToHexByteString $
                   hmacGetDigest
                     (hmac (Base64.encode $ secret tok) presign :: HMAC SHA256)
      return
        req
          { requestBody = RequestBodyBS rBody
          , requestHeaders =
              requestHeaders req ++
              [ ("cb-access-key", key tok)
              , ("cb-access-sign", sign)
              , ("cb-access-timestamp", time)
              ] ++
              if isForExchange
                then [("cb-access-passphrase", passphrase tok)]
                else [("cb-version", "2016-05-11")]
          }
    Nothing -> throwIO $ AuthenticationRequiredFailure $ T.pack p
  where
    pullBody (RequestBodyBS b)  = return b
    pullBody (RequestBodyLBS b) = return $ LBS.toStrict b
    pullBody _                  = throwIO AuthenticationRequiresByteStrings
signMessage _ False _ _ req = return req

--
processResponse ::
     (FromJSON b, MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => IsForExchange
  -> Response (ConduitM () BS.ByteString m ())
  -> m b
processResponse isForExchange res =
  case responseStatus res of
    s
      | s == status200 || (s == created201 && not isForExchange) -> do
        body <- responseBody res `connect` sinkParser (fmap fromJSON json)
        case body of
          Success b -> return b
          Error er  -> throwIO $ ParseFailure $ T.pack er
      | otherwise -> do
        body <- responseBody res `connect` CB.sinkLbs
        throwIO $ ApiFailure $ T.decodeUtf8 $ LBS.toStrict body

processPaginatedResponse ::
     (FromJSON b, MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => Response (ConduitM () BS.ByteString m ())
  -> m (b, Pagination)
processPaginatedResponse res =
  case responseStatus res of
    s
      | s == status200 -> do
        let headers = responseHeaders res
            pagination =
              Pagination
                { before = CBS.unpack <$> lookup "CB-BEFORE" headers
                , after = CBS.unpack <$> lookup "CB-AFTER" headers
                }
        body <- responseBody res `connect` sinkParser (fmap fromJSON json)
        case body of
          Success b -> return (b, pagination)
          Error er  -> throwIO $ ParseFailure $ T.pack er
      | otherwise -> do
        body <- responseBody res `connect` CB.sinkLbs
        throwIO $ ApiFailure $ T.decodeUtf8 $ LBS.toStrict body

processEmpty ::
     (MonadResource m, MonadReader ExchangeConf m)
  => Response (ConduitM () BS.ByteString m ())
  -> m ()
processEmpty res =
  case responseStatus res of
    s
      | s == status200 -> return ()
      | otherwise -> do
        body <- responseBody res `connect` CB.sinkLbs
        throwIO $ ApiFailure $ T.decodeUtf8 $ LBS.toStrict body
