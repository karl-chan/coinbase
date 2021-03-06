{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Coinbase.Exchange.Types.Socket where

-------------------------------------------------------------------------------
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson                   (encode)
import           Data.Aeson.Types             hiding (Error)
import           Data.Char
import           Data.Data
import           Data.Hashable
import qualified Data.HashMap.Strict          as H
import           Data.Text                    (Text)
import           Data.Time
import           GHC.Generics

-------------------------------------------------------------------------------
import           Coinbase.Exchange.Types.Core hiding (OrderStatus (..))

-------------------------------------------------------------------------------
-- | Fields to send along for websocket feed authentication
data Auth = Auth
  { authSignature  :: Text
  , authKey        :: Text
  , authPassphrase :: Text
  , authTimestamp  :: Text
  } deriving (Eq, Show, Read, Data, Typeable, Generic, NFData)

instance ToJSON Auth where
  toEncoding = genericToEncoding defaultOptions

-------------------------------------------------------------------------------
data Channel
  = Ticker
  | Level2
  | User
  | Matches
  | Fail
  | Full
  deriving (Eq, Show, Read, Enum, Bounded, Data, Typeable, Generic)

instance NFData Channel

instance Hashable Channel

instance ToJSON Channel where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance FromJSON Channel where
  parseJSON =
    genericParseJSON defaultOptions {constructorTagModifier = map toLower}

-------------------------------------------------------------------------------
-- | Messages we can send to the exchange
data SendExchangeMessage
  = Subscribe (Maybe Auth)
              [ProductId]
              [Channel]
  | SetHeartbeat Bool
  deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData SendExchangeMessage

-------------------------------------------------------------------------------
-- | Messages they send back to us
data ExchangeMessage
  = Heartbeat { msgTime        :: UTCTime
              , msgProductId   :: ProductId
              , msgSequence    :: Sequence
              , msgLastTradeId :: TradeId }
  | Subscriptions { msgChannels :: [ChannelSubscription] }
  | ReceivedLimit { msgTime      :: UTCTime
                  , msgProductId :: ProductId
                  , msgSequence  :: Sequence
                  , msgOrderId   :: OrderId
                  , msgSide      :: Side
                  , msgSize      :: Size
                  , msgPrice     :: Price }
  | ReceivedMarket { msgTime           :: UTCTime
                   , msgProductId      :: ProductId
                   , msgSequence       :: Sequence
                   , msgOrderId        :: OrderId
                   , msgSide           :: Side
                   , msgSizeAndOrFunds :: Either Size (Maybe Size, Cost) }
  | Open { msgTime          :: UTCTime
         , msgProductId     :: ProductId
         , msgSequence      :: Sequence
         , msgOrderId       :: OrderId
         , msgSide          :: Side
         , msgRemainingSize :: Size
         , msgPrice         :: Price }
  | Match { msgTime           :: UTCTime
          , msgProductId      :: ProductId
          , msgSequence       :: Sequence
          , msgSide           :: Side
          , msgTradeId        :: TradeId
          , msgMakerOrderId   :: OrderId
          , msgTakerOrderId   :: OrderId
          , msgSize           :: Size
          , msgPrice          :: Price
          , msgUserId         :: Maybe UserId
          , msgProfileId      :: Maybe ProfileId
          , msgTakerUserId    :: Maybe UserId
          , msgTakerProfileId :: Maybe ProfileId }
  | Done { msgTime         :: UTCTime
         , msgProductId    :: ProductId
         , msgSequence     :: Sequence
         , msgOrderId      :: OrderId
         , msgSide         :: Side
         , msgReason       :: Reason
        -- It is possible for these next two fields to be Nothing separately
        -- Filled market orders limited by funds will not have a price but may have remaining_size
        -- Filled limit orders may have a price but not a remaining_size (assumed zero)
        -- CURRENTLY ** `remaining_size` reported in Done messages is sometimes incorrect **
        -- This appears to be bug at GDAX. I've told them about it.
         , msgMaybePrice   :: Maybe Price
         , msgMaybeRemSize :: Maybe Size }
  | ChangeLimit { msgTime      :: UTCTime
                , msgProductId :: ProductId
                , msgSequence  :: Sequence
                , msgOrderId   :: OrderId
                , msgSide      :: Side
                , msgPrice     :: Price
                , msgNewSize   :: Size
                , msgOldSize   :: Size }
  | ChangeMarket { msgTime      :: UTCTime
                 , msgProductId :: ProductId
                 , msgSequence  :: Sequence
                 , msgOrderId   :: OrderId
                 , msgSide      :: Side
                 , msgNewFunds  :: Cost
                 , msgOldFunds  :: Cost }
  | Activate { msgTime         :: UTCTime
             , msgProductId    :: ProductId
             , msgTimestamp    :: NominalDiffTime
             , msgOrderId      :: OrderId
             , msgSide         :: Side
             , msgStopType     :: StopType
             , msgStopPrice    :: Price
             , msgSize         :: Size
             , msgFunds        :: Cost
             , msgTakerFeeRate :: CoinScientific
             , msgMaybePrivate :: Maybe Bool }
  | TickerMsg { msgTradeId   :: TradeId
              , msgSequence  :: Sequence
              , msgTime      :: UTCTime
              , msgProductId :: ProductId
              , msgPrice     :: Price
              , msgSide      :: Side
              , msgLastSize  :: Size
              , msgBestBid   :: Price
              , msgBestAsk   :: Price }
  | Snapshot { msgProductId :: ProductId
             , msgBids      :: [(Price, Size)]
             , msgAsks      :: [(Price, Size)] }
  | L2Update { msgProductId :: ProductId
             , msgChanges   :: [(Side, Price, Size)] }
  | Error { msgMessage :: Text }
  deriving (Eq, Show, Data, Typeable, Generic)

instance NFData ExchangeMessage

data ChannelSubscription = ChannelSubscription
  { msgName       :: Channel
  , msgProductIds :: [ProductId]
  } deriving (Eq, Show, Data, Typeable, Generic, NFData)

instance ToJSON ChannelSubscription where
  toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON ChannelSubscription where
  parseJSON = genericParseJSON coinbaseAesonOptions

-----------------------------
instance FromJSON ExchangeMessage where
  parseJSON (Object m) = do
    msgtype <- m .: "type"
        -- TO DO: `HeartbeatReq` and `Subscribe` message types are missing as those are
        -- never received by the client.
    case (msgtype :: String) of
      "hearbeat" ->
        Heartbeat <$> m .: "time" <*> m .: "product_id" <*> m .: "sequence" <*>
        m .: "last_trade_id"
      "subscriptions" -> Subscriptions <$> m .: "channels"
      "received" -> do
        typ <- m .: "order_type"
        case typ of
          Limit ->
            ReceivedLimit <$> m .: "time" <*> m .: "product_id" <*>
            m .: "sequence" <*>
            m .: "order_id" <*>
            m .: "side" <*>
            m .: "size" <*>
            m .: "price"
          Market ->
            ReceivedMarket <$> m .: "time" <*> m .: "product_id" <*>
            m .: "sequence" <*>
            m .: "order_id" <*>
            m .: "side" <*>
            (do ms <- m .:? "size"
                mf <- m .:? "funds"
                case (ms, mf) of
                  (Nothing, Nothing) -> mzero
                  (Just s, Nothing)  -> return $ Left s
                  (Nothing, Just f)  -> return $ Right (Nothing, f)
                  (Just s, Just f)   -> return $ Right (Just s, f))
      "open" ->
        Open <$> m .: "time" <*> m .: "product_id" <*> m .: "sequence" <*>
        m .: "order_id" <*>
        m .: "side" <*>
        m .: "remaining_size" <*>
        m .: "price"
      "match" ->
        Match <$> m .: "time" <*> m .: "product_id" <*> m .: "sequence" <*>
        m .: "side" <*>
        m .: "trade_id" <*>
        m .: "maker_order_id" <*>
        m .: "taker_order_id" <*>
        m .: "size" <*>
        m .: "price" <*>
        m .:? "user_id" <*>
        m .:? "profile_id" <*>
        m .:? "taker_user_id" <*>
        m .:? "taker_profile_id"
      "done" ->
        Done <$> m .: "time" <*> m .: "product_id" <*> m .: "sequence" <*>
        m .: "order_id" <*>
        m .: "side" <*>
        m .: "reason" <*>
        m .:? "price" <*>
        m .:? "remaining_size"
      "change" -> do
        maybePrice <- m .:? "price"
        case (maybePrice :: Maybe Price) of
          Just price ->
            ChangeLimit <$> m .: "time" <*> m .: "product_id" <*>
            m .: "sequence" <*>
            m .: "order_id" <*>
            m .: "side" <*>
            (pure price) <*>
            m .: "new_size" <*>
            m .: "old_size"
          Nothing ->
            ChangeMarket <$> m .: "time" <*> m .: "product_id" <*>
            m .: "sequence" <*>
            m .: "order_id" <*>
            m .: "side" <*>
            m .: "new_funds" <*>
            m .: "old_funds"
      "activate" ->
        Activate <$> m .: "time" <*> m .: "product_id" <*> m .: "timestamp" <*>
        m .: "order_id" <*>
        m .: "side" <*>
        m .: "stop_type" <*>
        m .: "stop_price" <*>
        m .: "size" <*>
        m .: "funds" <*>
        m .: "taker_fee_rate" <*>
        m .:? "private"
      "ticker" ->
        TickerMsg <$> m .: "trade_id" <*> m .: "sequence" <*> m .: "time" <*>
        m .: "product_id" <*>
        m .: "price" <*>
        m .: "side" <*>
        m .: "last_size" <*>
        m .: "best_bid" <*>
        m .: "best_ask"
      "snapshot" ->
        Snapshot <$> m .: "product_id" <*> m .: "bids" <*> m .: "asks"
      "l2update" -> L2Update <$> m .: "product_id" <*> m .: "changes"
      "error" -> Error <$> m .: "message"
      _ ->
        error $
        "Failed to decode exchange message: " ++ (show . encode $ Object m)
  parseJSON _ = mzero

---------------------------
-- This is based on the code for Aeson's (.:?) operator. Except, we're more
-- lax than (.:?) and also return 'Nothing' when the field is (JSON) null.
(.:??) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:?? key =
  case H.lookup key obj of
    Nothing -> pure Nothing
    Just v ->
      if v == Null
        then pure Nothing
        else obj .:? key

-------------------------------------------------------------------------------
instance ToJSON SendExchangeMessage where
  toJSON (Subscribe maybeAuth pids channels) =
    object $
    [ "type" .= ("subscribe" :: Text)
    , "product_ids" .= pids
    , "channels" .= channels
    ] ++
    authDetails
    where
      authDetails =
        case maybeAuth of
          Nothing -> []
          Just auth ->
            [ "signature" .= authSignature auth
            , "key" .= authKey auth
            , "passphrase" .= authPassphrase auth
            , "timestamp" .= authTimestamp auth
            ]
  toJSON (SetHeartbeat b) = object ["type" .= ("heartbeat" :: Text), "on" .= b]

--- | Convenience/storage instance; never sent to exchange
instance ToJSON ExchangeMessage where
  toJSON Heartbeat {..} =
    object $
    [ "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "last_trade_id" .= msgLastTradeId
    ]
  toJSON Subscriptions {..} =
    object $ ["type" .= ("subscriptions" :: Text), "channels" .= msgChannels]
  toJSON ReceivedLimit {..} =
    object $
    [ "type" .= ("received" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "size" .= msgSize
    , "price" .= msgPrice
    , "order_type" .= Limit
    ]
  toJSON ReceivedMarket {..} =
    object $
    [ "type" .= ("received" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    ] ++
    size ++ funds ++ ["order_type" .= Market]
    where
      (size, funds) =
        case msgSizeAndOrFunds of
          Left s -> (["size" .= s], [])
          Right (ms, f) ->
            case ms of
              Nothing -> ([], ["funds" .= f])
              Just s' -> (["size" .= s'], ["funds" .= f])
  toJSON Open {..} =
    object $
    [ "type" .= ("open" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "remaining_size" .= msgRemainingSize
    , "price" .= msgPrice
    ]
  toJSON Match {..} =
    object $
    [ "type" .= ("match" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "side" .= msgSide
    , "trade_id" .= msgTradeId
    , "maker_order_id" .= msgMakerOrderId
    , "taker_order_id" .= msgTakerOrderId
    , "size" .= msgSize
    , "price" .= msgPrice
    ] ++
    optionalField "user_id" msgUserId ++
    optionalField "profile_id" msgProfileId ++
    optionalField "taker_user_id" msgTakerUserId ++
    optionalField "taker_profile_id" msgTakerProfileId
  toJSON Done {..} =
    object $
    [ "type" .= ("done" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "reason" .= msgReason
    ] ++
    optionalField "price" msgMaybePrice ++
    optionalField "remaining_size" msgMaybeRemSize
  toJSON ChangeLimit {..} =
    object $
    [ "type" .= ("change" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "price" .= msgPrice
    , "new_size" .= msgNewSize
    , "old_size" .= msgOldSize
    ]
  toJSON ChangeMarket {..} =
    object $
    [ "type" .= ("change" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "sequence" .= msgSequence
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "new_funds" .= msgNewFunds
    , "old_funds" .= msgOldFunds
    ]
  toJSON Activate {..} =
    object $
    [ "type" .= ("activate" :: Text)
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "timestamp" .= msgTimestamp
    , "order_id" .= msgOrderId
    , "side" .= msgSide
    , "stop_type" .= msgStopType
    , "stop_price" .= msgStopPrice
    , "size" .= msgSize
    , "funds" .= msgFunds
    , "taker_fee_rate" .= msgTakerFeeRate
    ] ++
    optionalField "private" msgMaybePrivate
  toJSON TickerMsg {..} =
    object $
    [ "type" .= ("ticker" :: Text)
    , "trade_id" .= msgTradeId
    , "sequence" .= msgSequence
    , "time" .= msgTime
    , "product_id" .= msgProductId
    , "price" .= msgPrice
    , "side" .= msgSide
    , "last_size" .= msgLastSize
    , "best_bid" .= msgBestBid
    , "best_ask" .= msgBestAsk
    ]
  toJSON Snapshot {..} =
    object $
    [ "type" .= ("snapshot" :: Text)
    , "product_id" .= msgProductId
    , "bids" .= msgBids
    , "asks" .= msgAsks
    ]
  toJSON L2Update {..} =
    object $
    [ "type" .= ("l2update" :: Text)
    , "product_id" .= msgProductId
    , "changes" .= msgChanges
    ]
  toJSON Error {..} =
    object ["type" .= ("error" :: Text), "message" .= msgMessage]

-------------------------------------------------------------------------------
optionalField :: (ToJSON v) => Text -> Maybe v -> [Pair]
optionalField key maybeValue =
  case maybeValue of
    Nothing    -> []
    Just value -> [key .= value]
