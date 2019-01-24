{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This script streams live socket feed to stdout
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Network.WebSockets             as WS

import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Socket

main :: IO ()
main = printSocket

printSocket :: IO ()
printSocket = do
  subscribe Nothing Live ["BTC-USD"] [Full] $ \conn -> do
    putStrLn "Connected."
    _ <-
      forkIO $
      forever $ do
        ds <- WS.receiveData conn
        let res = eitherDecode ds
        case res :: Either String ExchangeMessage of
          Left er -> print er
          Right v -> print v
    _ <- forever $ threadDelay (1000000 * 60)
    return ()
