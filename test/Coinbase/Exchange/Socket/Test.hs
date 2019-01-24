module Coinbase.Exchange.Socket.Test
  ( tests
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Aeson

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Network.WebSockets             as WS

import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket

-------------------------------------
-- NOTE: [Connectivity Precondition]
--
-- The parsing tests are time-based and assume we are receiving messages during the
-- time we are connected. However, those tests are NOT FAILSAFE.
--
-- ** If no data is received, parsing succeeds and, therefore, the parsing tests succeed **
--
-- To ensure this unsafe behavior does not go unnoticed (we thinking we are
-- parsing correctly when, in fact, we are not parsing anything at all),
-- We first verify we can receive at least 20 messages (i.e. a fixed minimum number)
-- from the socket, before running the parsing tests.
-------------------------------------
tests :: (Maybe ExchangeConf) -> [ProductId] -> [Channel] -> TestTree
tests maybeConf products channels =
  testGroup
    "Socket"
        -- See NOTE: [Connectivity Precondition]
    [ testCase
        "Do I receive messages?"
        (receiveSocket maybeConf products channels)
    , testCase
        "Parse Websocket Stream"
        (parseSocket maybeConf products channels (threadDelay $ 1000000 * 20))
    , testCase
        "Decode Re-Encode Decode"
        (reencodeSocket maybeConf products channels)
    ]

receiveSocket :: (Maybe ExchangeConf) -> [ProductId] -> [Channel] -> IO ()
receiveSocket maybeConf products channels =
  subscribe maybeConf Live products channels $ \conn -> do
    sequence_ $ replicate 20 (receiveAndDecode conn)

-- Success: no parse errors   found while running
-- Failure: a parse error is  found while running
parseSocket :: (Maybe ExchangeConf) -> [ProductId] -> [Channel] -> IO a -> IO ()
parseSocket maybeConf products channels challenge =
  subscribe maybeConf Live products channels $ \conn -> do
    _ <- waitCancelThreads challenge (forever $ receiveAndDecode conn)
    return ()

-- FIX ME! there's no guarantee we are hitting all order types.
-- a more thorough test would be better.
reencodeSocket :: (Maybe ExchangeConf) -> [ProductId] -> [Channel] -> IO ()
reencodeSocket maybeConf products channels =
  subscribe maybeConf Live products channels $ \conn -> do
    sequence_ $ replicate 1000 (decodeEncode conn)

decodeEncode :: WS.Connection -> IO ()
decodeEncode conn = do
  ds <- WS.receiveData conn
  let res = eitherDecode ds
  case res :: Either String ExchangeMessage of
    Left er -> assertFailure "Failure parsing data from exchange" >> print er
    Right received -> do
      let enc = encode received
          dec = eitherDecode enc
      if dec == res
        then return ()
        else do
          putStrLn $ "### original: " ++ show res
          putStrLn $ "### obtained: " ++ show dec

receiveAndDecode :: WS.Connection -> IO ()
receiveAndDecode conn = do
  ds <- WS.receiveData conn
  let res = eitherDecode ds {- $ trace (show ds) -}
  case res :: Either String ExchangeMessage of
    Left er -> print er >> assertFailure "Parsing failure found"
    Right _ -> return ()

waitCancelThreads :: IO a -> IO b -> IO (Either a b)
waitCancelThreads action1 action2 = do
  a <- async action1
  b <- async action2
  c <- waitEither a b
  case c of
    Left _  -> cancel b
    Right _ -> cancel a
  return c
