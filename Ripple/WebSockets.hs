module Ripple.WebSockets (
	-- * Base WebSocket helpers
	receiveJSON,
	sendJSON,
	-- * Ripple JSON result parsing and error handling
	RippleError(..),
	RippleResult(..),
	getRippleResult,
	getRippleResult',
	-- * ripple_path_find
	CommandRipplePathFind(..),
	ResultRipplePathFind(..),
	Alternative(..),
	-- * account_tx
	CommandAccountTX(..),
	ResultAccountTX(..),
	-- * ledger_closed
	CommandLedgerClosed(..),
	ResultLedgerClosed(..)
) where

import Numeric (readHex)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM)
import Control.Error (note, fmapL, readZ, justZ)
import Data.Base58Address (RippleAddress)
import Data.Binary (decodeOrFail)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LZ

import Ripple.Transaction
import Ripple.Amount

-- Base WebSocket helpers

receiveJSON :: (Aeson.FromJSON j, MonadIO m) => WS.Connection -> m (Either String j)
receiveJSON = liftIO . fmap Aeson.eitherDecode . WS.receiveData

sendJSON :: (Aeson.ToJSON j, MonadIO m) => WS.Connection -> j -> m ()
sendJSON conn = liftIO . WS.sendTextData conn . Aeson.encode

-- Ripple JSON result parsing and error handling

-- | Ripple server error codes
data RippleError =
	UnknownCommand |
	ResponseParseError String |
	OtherRippleError Int String String
	deriving (Show, Eq)

-- | The result of a WebSocket command -- either error or a response
data RippleResult id a = RippleResult (Maybe id) (Either RippleError a)
	deriving (Show, Eq)

getRippleResult' :: Either String (RippleResult id a) -> Either RippleError a
getRippleResult' (Left e) = Left $ ResponseParseError e
getRippleResult' (Right (RippleResult _ x)) = x

getRippleResult :: Either String (RippleResult () a) -> Either RippleError a
getRippleResult = getRippleResult'

instance (Aeson.FromJSON a, Aeson.FromJSON id) =>
		Aeson.FromJSON (RippleResult id a) where
	parseJSON (Aeson.Object o) = RippleResult <$> o .:? T.pack "id" <*> do
		status <- o .: T.pack "status"
		typ <- o .: T.pack "type"
		case (status, typ) of
			("success", "response") ->
				Right <$> (Aeson.parseJSON =<< o.: T.pack "result")
			("error", "response") -> do
				err  <- o .: T.pack "error"
				code <- o .: T.pack "error_code"
				msg  <- o .: T.pack "error_message"
				case code of
					27 -> return $ Left UnknownCommand
					_ -> return $ Left $ OtherRippleError code err msg
			_ -> fail "Invalid Ripple Result"
	parseJSON _ = fail "Ripple Result is always a JSON object"

-- ripple_path_find

data CommandRipplePathFind = CommandRipplePathFind {
		source_account :: RippleAddress,
		destination_account :: RippleAddress,
		destination_amount :: Amount
	} deriving (Show, Eq)

instance Aeson.ToJSON CommandRipplePathFind where
	toJSON (CommandRipplePathFind source dest amount) = Aeson.object [
			T.pack "command" .= T.pack "ripple_path_find",
			T.pack "source_account" .= show source,
			T.pack "destination_account" .= show dest,
			T.pack "destination_amount" .= amount
		]

data ResultRipplePathFind = ResultRipplePathFind {
		alternatives :: [Alternative],
		response_destination_account :: RippleAddress
	} deriving (Show, Eq)

instance Aeson.FromJSON ResultRipplePathFind where
	parseJSON (Aeson.Object o) = ResultRipplePathFind <$>
		o .: T.pack "alternatives" <*>
		(readZ =<< o .: T.pack "destination_account")
	parseJSON _ = fail "PathFindResponse is always a JSON object"

data Alternative = Alternative {
		source_amount :: Amount
	} deriving (Show, Eq)

instance Aeson.FromJSON Alternative where
	parseJSON (Aeson.Object o) = Alternative <$> o .: T.pack "source_amount"
	parseJSON _ = fail "Alternative is always a JSON object"

-- account_tx

data CommandAccountTX = CommandAccountTX {
		account        :: RippleAddress,
		limit          :: Int,
		offset         :: Maybe Int,
		ledgerIndexMin :: Maybe Integer,
		ledgerIndexMax :: Maybe Integer,
		descending     :: Bool,
		binary         :: Bool
	}

instance Aeson.ToJSON CommandAccountTX where
	toJSON (CommandAccountTX account lim off min max desc bin) = Aeson.object [
			T.pack "command"          .= "account_tx",
			T.pack "account"          .= show account,
			T.pack "ledger_index_min" .= fromMaybe (-1) min,
			T.pack "ledger_index_max" .= fromMaybe (-1) max,
			T.pack "binary"           .= bin,
			T.pack "limit"            .= lim,
			T.pack "offset"           .= fromMaybe 0 off,
			T.pack "descending"       .= desc
		]

-- | [(ledger_index, transaction+meta)]
data ResultAccountTX = ResultAccountTX [(Integer,Transaction)]
	deriving (Show, Eq)

instance Aeson.FromJSON ResultAccountTX where
	parseJSON (Aeson.Object o) = ResultAccountTX <$> do
		transactions <- o .: T.pack "transactions"
		forM transactions $ \transaction -> do
			True   <- transaction .: T.pack "validated"
			mblob  <- transaction .:? T.pack "tx_blob" -- binary transaction
			mtx    <- transaction .:? T.pack "tx" -- json transaction

			case (mblob, mtx) of
				(Just blob, Nothing) -> do
					meta <- transaction .: T.pack "meta"
					tr <- either fail return $ do
						bytes <- note "Invalid Hexidecimal encoding" $ hex2bytes blob
						Transaction tr <- fmapL (\(_,_,e)->e) $ fmap (\(_,_,r)->r) $
							decodeOrFail (LZ.pack bytes)

						bytes <- note "Invalid Hexidecimal encoding" $ hex2bytes meta
						Transaction mta <- fmapL (\(_,_,e)->e) $ fmap (\(_,_,r)->r) $
							decodeOrFail (LZ.pack bytes)

						return $ Transaction (tr ++ mta)

					ledger <- transaction .: T.pack "ledger_index"
					return (ledger, tr)

				(Nothing, Just (Transaction tx)) -> do
					Transaction meta <- transaction .: T.pack "meta"
					Aeson.Object txo <- transaction .: T.pack "tx"
					ledger <- txo .: T.pack "ledger_index"
					return (ledger, Transaction (tx ++ meta))

				(Just _, Just _) -> fail "tx or tx_blob required (not both)"
				_ -> fail "tx or tx_blob required"

	parseJSON _ = fail "account_tx result is always a JSON object"

-- ledger_closed

data CommandLedgerClosed = CommandLedgerClosed
	deriving (Show, Eq)

instance Aeson.ToJSON CommandLedgerClosed where
	toJSON CommandLedgerClosed = Aeson.object [
			T.pack "command" .= "ledger_closed"
		]

data ResultLedgerClosed = ResultLedgerClosed LZ.ByteString Integer
	deriving (Show, Eq)

instance Aeson.FromJSON ResultLedgerClosed where
	parseJSON (Aeson.Object o) = ResultLedgerClosed <$>
		(fmap LZ.pack . justZ . hex2bytes =<< o .: T.pack "ledger_hash") <*>
		(o .: T.pack "ledger_index")
	parseJSON _ = fail "ledger_closed result is always a JSON object"

-- Helpers

hex2bytes :: String -> Maybe [Word8]
hex2bytes [] = Just []
hex2bytes (x:y:rest) = case readHex [x,y] of
	[(n, "")] -> fmap (n:) (hex2bytes rest)
	_ -> Nothing
hex2bytes _ = Nothing
