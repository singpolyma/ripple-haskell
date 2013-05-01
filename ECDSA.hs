module ECDSA where

import qualified Data.ByteString as BS

import Crypto.Types.PubKey.ECDSA (PrivateKey(..), PublicKey(..))
import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), Point(..))

import Codec.Crypto.ECC.Base (pmul, getx, modinv)

import Crypto.Random (genBytes, GenError, CryptoRandomGen)
import Crypto.Util (bs2i)

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
