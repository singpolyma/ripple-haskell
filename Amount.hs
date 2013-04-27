module Amount where

import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Word
import Data.Binary (Binary(..), Get, putWord8)
import Data.Binary.Get (getLazyByteString)
import Data.Base58Address (RippleAddress)
import qualified Data.ByteString.Lazy as LZ

data Currency = XRP | Currency (Char,Char,Char) RippleAddress
	deriving (Eq)

instance Show Currency where
	show XRP = "XRP"
	show (Currency (a,b,c) adr) = [a,b,c,'/'] ++ show adr

instance Binary Currency where
	get = do
		allZero <- getLazyByteString 12
		currency <- getLazyByteString 3
		version <- getLazyByteString 2
		reserved <- getLazyByteString 3
		issuer <- get

		when (LZ.any (/=0) allZero) (fail "Bad currency format az")
		when (LZ.any (/=0) version) (fail "Bad currency format ver")
		when (LZ.any (/=0) reserved) (fail "Bad currency format res")

		-- Spec says ASCII
		let [a,b,c] = map (toEnum.fromIntegral) $ LZ.unpack currency
		return $ Currency (a,b,c) issuer

	put XRP = fail "XRP does not get encoded as a currency specifier."
	put (Currency (a,b,c) issuer) = do
		replicateM_ 12 (putWord8 0)
		putWord8 $ fromIntegral $ fromEnum a
		putWord8 $ fromIntegral $ fromEnum b
		putWord8 $ fromIntegral $ fromEnum c
		replicateM_ 2 (putWord8 0)
		replicateM_ 3 (putWord8 0)
		put issuer

data Amount = Amount Rational Currency
	deriving (Eq)

instance Show Amount where
	show (Amount a c) =
		show (realToFrac a :: Double) ++ "/" ++ show c

instance Binary Amount where
	get = do
		value <- get :: Get Word64
		if testBit value 63 then
			(flip Amount <$> get <*>) $ pure $
			case (clearBit (clearBit value 63) 62 `shiftR` 54, value .&. 0x003FFFFFFFFFFFFF) of
				(0,0) -> 0
				(e,m) ->
					(if testBit value 62 then 1 else -1) *
					fromIntegral m * (10 ^^ (fromIntegral e - 97))
		else
			return $ (`Amount` XRP) $
			(if testBit value 62 then 1 else -1) *
			(fromIntegral (clearBit value 62) / 1000000)

	put (Amount value XRP) =
		put $ (if value >= 0 then (`setBit` 62) else id) drops
		where
		drops = floor $ abs $ value * 1000000 :: Word64
	put (Amount 0 currency) = do
		put (setBit (0 :: Word64) 63)
		put currency
	put (Amount value currency)
		| value > 0 = put (setBit encoded 62) >> put currency
		| otherwise = put encoded >> put currency
		where
		encoded = setBit ((e8 `shiftL` 54) .|. m64) 63
		e8 = fromIntegral (fromIntegral (e+97) :: Word8) -- to get the bits
		m64 = fromIntegral m :: Word64
		(m,e) = until ((> man_min_value) . fst) (\(m,e) -> (m*10,e-1)) $
			until ((< man_max_value) . fst) (\(m,e) -> (m`div`10,e+1)) $
			(abs $ floor (value * (10 ^^ 80)), -80)

man_max_value :: Integer
man_max_value = 9999999999999999

man_min_value :: Integer
man_min_value = 1000000000000000
