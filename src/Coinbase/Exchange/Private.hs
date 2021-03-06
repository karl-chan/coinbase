{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Private
  ( getAccountList
  , getAccount
  , getAccountLedger
  , getAccountHolds
  , createOrder
  , cancelOrder
  , cancelAllOrders
  , getOrderList
  , getOrder
  , getFills
  , createTransfer
  , createCryptoWithdrawal
  , createReport
  , getReportStatus
  , module Coinbase.Exchange.Types.Private
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Char
import           Data.List
import qualified Data.Text                       as T
import           Data.UUID

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private

-- Accounts
getAccountList ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m) => m [Account]
getAccountList = coinbaseGet True "/accounts"

getAccount ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => AccountId
  -> m Account
getAccount (AccountId i) = coinbaseGet True ("/accounts/" ++ toString i)

getAccountLedger ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => AccountId
  -> m [Entry]
getAccountLedger (AccountId i) =
  coinbaseGet True ("/accounts/" ++ toString i ++ "/ledger")

getAccountHolds ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => AccountId
  -> m [Hold]
getAccountHolds (AccountId i) =
  coinbaseGet True ("/accounts/" ++ toString i ++ "/holds")

-- Orders
createOrder ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => NewOrder
  -> m OrderId
createOrder = liftM ocId . coinbasePost True "/orders" . Just

cancelOrder ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => OrderId
  -> m ()
cancelOrder (OrderId o) =
  coinbaseDeleteDiscardBody True ("/orders/" ++ toString o) voidBody

cancelAllOrders ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => Maybe ProductId
  -> m [OrderId]
cancelAllOrders prodId = coinbaseDelete True ("/orders" ++ opt prodId) voidBody
  where
    opt Nothing    = ""
    opt (Just pid) = "?product_id=" ++ T.unpack (unProductId pid)

getOrderList ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => [OrderStatus]
  -> m [Order]
getOrderList os = coinbaseGet True ("/orders?" ++ query os)
  where
    query [] = "status=open&status=pending&status=active"
    query xs =
      intercalate "&" $ map (\x -> "status=" ++ map toLower (show x)) xs

getOrder ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => OrderId
  -> m Order
getOrder (OrderId o) = coinbaseGet True ("/orders/" ++ toString o)

-- Fills
getFills ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => Maybe OrderId
  -> Maybe ProductId
  -> m [Fill]
getFills moid mpid = coinbaseGet True ("/fills?" ++ oid ++ "&" ++ pid)
  where
    oid =
      case moid of
        Just v  -> "order_id=" ++ toString (unOrderId v)
        Nothing -> ""
    pid =
      case mpid of
        Just v  -> "product_id=" ++ T.unpack (unProductId v)
        Nothing -> ""

-- Transfers
createTransfer ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => TransferToCoinbase
  -> m TransferToCoinbaseResponse
createTransfer = coinbasePost True "/transfers" . Just

createCryptoWithdrawal ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => CryptoWithdrawal
  -> m CryptoWithdrawalResp
createCryptoWithdrawal = coinbasePost True "/withdrawals/crypto" . Just

-- Reports
createReport ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => ReportRequest
  -> m ReportInfo
createReport = coinbasePost True "/reports" . Just

getReportStatus ::
     (MonadResource m, MonadReader ExchangeConf m, MonadThrow m)
  => ReportId
  -> m ReportInfo
getReportStatus (ReportId r) = coinbaseGet True ("/reports/" ++ toString r)
