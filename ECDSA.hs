module ECDSA where

import Data.Bits
import qualified Data.ByteString as BS

import Crypto.Types.PubKey.ECDSA (PrivateKey(..), PublicKey(..))
import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), Point(..))

import Codec.Crypto.ECC.Base (pmul, getx, modinv)

import Crypto.Random (genBytes, GenError, CryptoRandomGen)
import Crypto.Util (bs2i, i2bs)

import IntegerBytes (unroll)
import Hecc

sign :: (CryptoRandomGen g) => PrivateKey -> BS.ByteString -> g -> Either GenError ((Integer, Integer), g)
sign (PrivateKey curve@(CurveFP (CurvePrime _ (CurveCommon {ecc_g = g, ecc_n = n}))) d) hash gen = do
	(bytes, gen') <- genBytes l gen
	let k = (bs2i bytes `mod` (n-1)) + 1
	let r = getx (pmul (point2hecc curve g) k) `mod` n
	let s = ((bs2i hash + (r*d)) * modinv k n) `mod` n
	return ((r,s), gen')
	where
	l = length (unroll n) -- ByteLength
sign _ _ _ = error "TODO: binary curves"

publicFromPrivate :: PrivateKey -> PublicKey
publicFromPrivate (PrivateKey curve@(CurveFP (CurvePrime _ (CurveCommon {ecc_g = g}))) d) =
	PublicKey curve (hecc2point $ pmul (point2hecc curve g) d)
publicFromPrivate _ = error "TODO: binary curves"

-- This is used in Ripple, not sure if standard
publicToBytes :: PublicKey -> BS.ByteString
publicToBytes (PublicKey (CurveFP (CurvePrime _ (CurveCommon {ecc_n = n}))) (Point x y)) =
	BS.singleton (if y `mod` 2 == 0 then 0x02 else 0x03)
	`BS.append`
	i2bs (8 * length (unroll n)) x
publicToBytes _ = error "TODO: binary curves"

signatureEncodeDER :: (Integer, Integer) -> BS.ByteString
signatureEncodeDER (r,s) = BS.concat [
		BS.singleton 0x30,
		BS.singleton (fromIntegral $ 4 + length rb' + length sb'),
		BS.singleton 0x02,
		BS.singleton (fromIntegral $ length rb'),
		BS.pack rb',
		BS.singleton 0x02,
		BS.singleton (fromIntegral $ length sb'),
		BS.pack sb'
	]
	where
	-- If high bit is set, prepend an extra zero byte (DER signed integer)
	rb' | head rb .&. 0x80 /= 0 = 0:rb
	    | otherwise = rb

	sb' | head sb .&. 0x80 /= 0 = 0:sb
	    | otherwise = sb

	rb = unroll r
	sb = unroll s
