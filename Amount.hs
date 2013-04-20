module Amount where

import Data.Bits
import Data.Word
import Data.Binary (Binary(..), Get)
import Data.Base58Address (RippleAddress)

data Currency = XRP | Currency (Char,Char,Char) RippleAddress
	deriving (Eq)

instance Show Currency where
	show XRP = "XRP"
	show (Currency (a,b,c) adr) = [a,b,c,'/'] ++ show adr

data Amount = Amount Rational Currency
	deriving (Eq)

instance Show Amount where
	show (Amount a c) =
		show (realToFrac a :: Double) ++ "/" ++ show c

instance Binary Amount where
	get = do
		value <- get :: Get Word64
		if testBit value 63 then
			return $ Amount (-0.111) XRP -- TODO
			{-
			case ((clearBit value 63 `shiftR` 7) - 124, value .&. 0x00FFFFFFFFFFFFFF) of
				(0,0) -> Amount 0 XRP
				(e,m) -> Amount ((fromIntegral m) ^ e) XRP
			-}
		else
			return $ (`Amount` XRP) $
			(if testBit value 62 then 1 else -1) *
			(fromIntegral (clearBit value 62) / 1000000)

	put (Amount value XRP) =
		put $ (if value >= 0 then (`setBit` 62) else id) (setBit drops 63)
		where
		drops = floor $ abs $ value * 1000000 :: Word64
