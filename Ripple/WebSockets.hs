module Ripple.WebSockets (
	-- * Base WebSocket helpers
	receiveJSON,
	sendJSON,
	-- * Ripple JSON result parsing and error handling
	RippleError(..),
	RippleResult(..),
	-- * ripple_path_find
	CommandRipplePathFind(..),
	ResultRipplePathFind(..),
	Alternative(..),
	-- * account_tx
	CommandAccountTX(..),
	ResultAccountTX(..)
) where

import Numeric (readHex)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM)
import Control.Error (note, fmapL, readZ)
import Data.Base58Address (RippleAddress)
import Data.Binary (decodeOrFail)

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LZ

import Ripple.Transaction
import Ripple.Amount

-- Base WebSocket helpers

receiveJSON :: (Aeson.FromJSON j) => WS.Connection -> IO (Either String j)
receiveJSON = fmap Aeson.eitherDecode . WS.receiveData

sendJSON :: (Aeson.ToJSON j) => WS.Connection -> j -> IO ()
sendJSON conn = WS.sendTextData conn . Aeson.encode

-- Ripple JSON result parsing and error handling

-- | Ripple server error codes
data RippleError =
	UnknownCommand |
	OtherRippleError Int String String
	deriving (Show, Eq)

-- | The result of a WebSocket command -- either error or a response
newtype RippleResult a = RippleResult (Either RippleError a)
	deriving (Show, Eq)

instance (Aeson.FromJSON a) => Aeson.FromJSON (RippleResult a) where
	parseJSON (Aeson.Object o) = RippleResult <$> do
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
					27 -> return $ Left $ UnknownCommand
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
		ledgerIndexMin :: Maybe Int,
		ledgerIndexMax :: Maybe Int,
		descending     :: Bool,
		binary         :: Bool
	}

instance Aeson.ToJSON CommandAccountTX where
	toJSON (CommandAccountTX account lim min max desc bin) = Aeson.object [
			T.pack "command"          .= "account_tx",
			T.pack "account"          .= show account,
			T.pack "ledger_index_min" .= fromMaybe (-1) min,
			T.pack "ledger_index_max" .= fromMaybe (-1) max,
			T.pack "binary"           .= bin,
			T.pack "limit"            .= lim,
			T.pack "descending"       .= desc
		]

-- | [(ledger_index, transaction)]
data ResultAccountTX = ResultAccountTX [(Integer,Transaction)]
	deriving (Show, Eq)

instance Aeson.FromJSON ResultAccountTX where
	parseJSON (Aeson.Object o) = ResultAccountTX <$> do
		transactions <- o .: T.pack "transactions"
		forM transactions $ \transaction -> do
			True <- transaction .: T.pack "validated"
			ledger <- transaction .: T.pack "ledger_index"
			blob <- transaction .:? T.pack "tx_blob" -- binary transaction

			tr <- either fail return $ do
				hex <- note "JSON transactions not implemented yet" blob
				bytes <- note "Invalid Hexidecimal encoding" $ hex2bytes hex
				fmapL (\(_,_,e) -> e) $ fmap (\(_,_,r) -> r) $
					decodeOrFail (LZ.pack bytes)

			return (ledger, tr)
	parseJSON _ = fail "account_tx result is always a JSON object"

-- Helpers

hex2bytes :: String -> Maybe [Word8]
hex2bytes [] = Just []
hex2bytes (x:y:rest) = case readHex [x,y] of
	[(n, "")] -> fmap (n:) (hex2bytes rest)
	_ -> Nothing
hex2bytes _ = Nothing