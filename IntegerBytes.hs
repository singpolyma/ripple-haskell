module IntegerBytes where

import Data.Word
import Data.Sequence (unfoldl)
import Data.Foldable (toList)

-- TODO: this doesn't really belong in this library

unroll :: Integer -> [Word8]
unroll = toBase 256

toBase :: (Integral a, Integral b) => a -> a -> [b]
toBase _ 0 = [0]
toBase b v
	| v < 0 = error "toBase v < 0"
	| otherwise = map fromIntegral $ toList $
		unfoldl (\n -> if n == 0 then Nothing else Just $! (n `divMod` b)) v
