module PathFind where

import Control.Applicative hiding (Alternative)
import Control.Error (readZ)

import Data.Base58Address (RippleAddress)
import qualified Data.Text as T

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson

import Amount

-- TODO: support all fields

data PathFindRequest = PathFindRequest {
		source_account :: RippleAddress,
		destination_account :: RippleAddress,
		destination_amount :: Amount
	} deriving (Show, Eq)

instance Aeson.ToJSON PathFindRequest where
	toJSON (PathFindRequest source dest amount) = Aeson.object [
			T.pack "command" .= T.pack "ripple_path_find",
			T.pack "source_account" .= show source,
			T.pack "destination_account" .= show dest,
			T.pack "destination_amount" .= amount
		]

data Alternative = Alternative {
		source_amount :: Amount
	} deriving (Show, Eq)

instance Aeson.FromJSON Alternative where
	parseJSON (Aeson.Object o) = Alternative <$> o .: T.pack "source_amount"
	parseJSON _ = fail "Alternative is always a JSON object"

data PathFindResponse = PathFindResponse {
		alternatives :: [Alternative],
		response_destination_account :: RippleAddress
	} deriving (Show, Eq)

instance Aeson.FromJSON PathFindResponse where
	parseJSON (Aeson.Object o) = PathFindResponse <$>
		o .: T.pack "alternatives" <*>
		(readZ =<< o .: T.pack "destination_account")
	parseJSON _ = fail "PathFindResponse is always a JSON object"
