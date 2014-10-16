module Ripple.Amount (
	Amount(..),
	Currency(..),
	CurrencySpecifier(..)
) where

import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Word
import Data.Binary (Binary(..), Get, putWord8)
import Data.Binary.Get (getLazyByteString)
import Data.Base58Address (RippleAddress)
import Control.Error (readZ)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Text as T

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

data Currency = XRP | Currency (Char,Char,Char) RippleAddress
	deriving (Eq)

instance Show Currency where
	show XRP = "XRP"
	show (Currency (a,b,c) adr) = [a,b,c,'/'] ++ show adr

instance Aeson.ToJSON Currency where
	toJSON XRP = Aeson.object [T.pack "currency" .= "XRP"]
	toJSON (Currency (a,b,c) issuer) = Aeson.object [
			T.pack "currency" .= [a,b,c],
			T.pack "issuer" .= show issuer
		]

instance Binary Currency where
	get = do
		CurrencySpecifier code <- get
		issuer <- get

		return $ Currency code issuer

	put XRP = fail "XRP does not get encoded as a currency specifier."
	put (Currency code issuer) = do
		put $ CurrencySpecifier code
		put issuer

-- | The raw 160-bit currency specifier, no issuer
newtype CurrencySpecifier = CurrencySpecifier (Char,Char,Char)
	deriving (Show, Eq)

instance Binary CurrencySpecifier where
	get = do
		allZero <- getLazyByteString 12
		currency <- getLazyByteString 3
		version <- getLazyByteString 2
		reserved <- getLazyByteString 3

		when (LZ.any (/=0) allZero) (fail "Bad currency format az")
		when (LZ.any (/=0) version) (fail "Bad currency format ver")
		when (LZ.any (/=0) reserved) (fail "Bad currency format res")

		-- Spec says ASCII
		let [a,b,c] = map (toEnum.fromIntegral) $ LZ.unpack currency

		return $ CurrencySpecifier (a,b,c)

	put (CurrencySpecifier (a,b,c)) = do
		replicateM_ 12 (putWord8 0)
		putWord8 $ fromIntegral $ fromEnum a
		putWord8 $ fromIntegral $ fromEnum b
		putWord8 $ fromIntegral $ fromEnum c
		replicateM_ 2 (putWord8 0)
		replicateM_ 3 (putWord8 0)

data Amount = Amount Rational Currency
	deriving (Eq)

instance Show Amount where
	show (Amount a c) =
		show (realToFrac a :: Double) ++ "/" ++ show c

instance Aeson.ToJSON Amount where
	toJSON (Amount v XRP) = Aeson.toJSON $ show (floor (v * one_drop) :: Integer)
	toJSON (Amount v (Currency (a,b,c) issuer)) = Aeson.object [
			T.pack "value" .= show (realToFrac v :: Double),
			T.pack "currency" .= [a,b,c],
			T.pack "issuer" .= show issuer
		]

instance Aeson.FromJSON Amount where
	parseJSON (Aeson.Object o) = do
		amountVal <- o .: T.pack "value"
		amount <- realToFrac <$> case amountVal of
			Aeson.Number n ->
				Aeson.parseJSON (Aeson.Number n) :: Aeson.Parser Double
			Aeson.String s ->
				readZ (T.unpack s) :: Aeson.Parser Double
			_ -> fail "No valid amount"
		currency <- o .: T.pack "currency"
		guard (length currency == 3 && currency /= "XRP")
		issuer <- readZ =<< o .: T.pack "issuer"

		let [a,b,c] = currency
		return $ Amount amount (Currency (a,b,c) issuer)
	parseJSON (Aeson.Number n)
		| floor n == ceiling n = pure $ Amount (realToFrac n / one_drop) XRP
		| otherwise = pure $ Amount (realToFrac n) XRP
	parseJSON (Aeson.String s) = case T.find (=='.') s of
		Nothing -> (Amount . (/one_drop) . realToFrac) <$>
			(readZ (T.unpack s) :: Aeson.Parser Integer) <*> pure XRP
		Just _ -> (\x -> Amount (realToFrac x)) <$>
			(readZ (T.unpack s) :: Aeson.Parser Double) <*> pure XRP
	parseJSON _ = fail "Invalid amount"

instance Binary Amount where
	get = do
		value <- get :: Get Word64
		if testBit value 63 then
			(flip Amount <$> get <*>) $ pure $
			case (clearBit (clearBit value 63) 62 `shiftR` 54, value .&. 0x003FFFFFFFFFFFFF) of
				(0,0) -> 0
				(e,m) ->
					(if testBit value 62 then 1 else -1) *
					fromIntegral m * (10 ^^ (fromIntegral e + exp_min - 1))
			else
				return $ (`Amount` XRP) $
				(if testBit value 62 then 1 else -1) *
				(fromIntegral (clearBit value 62) / one_drop)

	put (Amount value XRP) =
		put $ (if value >= 0 then (`setBit` 62) else id) drops
		where
		drops = floor $ abs $ value * one_drop :: Word64
	put (Amount 0 currency) = do
		put (setBit (0 :: Word64) 63)
		put currency
	put (Amount value currency)
		| value > 0 = put (setBit encoded 62) >> put currency
		| otherwise = put encoded >> put currency
		where
		encoded = setBit ((e8 `shiftL` 54) .|. m64) 63
		e8 = fromIntegral (fromIntegral (e-exp_min+1) :: Word8) -- to get the bits
		m64 = fromIntegral m :: Word64
		(m,e) = until ((>= man_min_value) . fst) (\(m,e) -> (m*10,e-1)) $
			until ((<= man_max_value) . fst) (\(m,e) -> (m`div`10,e+1))
			(abs $ floor (value * (10 ^^ exp_max)), -exp_max)

one_drop :: Rational
one_drop = 1000000

exp_max :: Integer
exp_max = 80

exp_min :: Integer
exp_min = -96

man_max_value :: Integer
man_max_value = 9999999999999999

man_min_value :: Integer
man_min_value = 1000000000000000
