module Ripple.Seed (getSecret) where

import Data.Word
import Data.Base58Address (RippleAddress, rippleAddressPayload)
import Crypto.Hash.CryptoAPI (SHA512, hash')
import qualified Data.ByteString as BS
import qualified Data.Serialize as Serialize

import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), getCurveByName, CurveName(SEC_p256k1))

import Crypto.Util (bs2i, i2bs_unsized)

import ECDSA (publicFromPrivate, publicToBytes, PrivateKey(..))

-- n is the order of the base point
n :: Integer
p256k1 :: Curve
p256k1@(CurveFP (CurvePrime _ (CurveCommon {ecc_n = n}))) = getCurveByName SEC_p256k1

-- | Derive the secret key for the given secret seed and address
getSecret ::
	RippleAddress -- ^ Secret seed address
	-> PrivateKey
getSecret seed = PrivateKey p256k1 d
	where
	d = (sec + priv) `mod` n
	sec = bs2i $ gen (pub `BS.append` seq)
	pub = publicToBytes $ publicFromPrivate $ PrivateKey p256k1 priv
	priv = bs2i $ gen sbytes
	sbytes = i2bs_unsized (rippleAddressPayload seed)
	seq = Serialize.encode (0 :: Word32)

gen :: BS.ByteString -> BS.ByteString
gen bytes = fst $ until (\(x,_) -> n >= bs2i x) (\(_,i) -> (go i,i+1)) (go 0, 1)
	where
	go i = halfOfSHA512 $ hash' (bytes `BS.append` Serialize.encode (i :: Word32))

halfOfSHA512 :: SHA512 -> BS.ByteString
halfOfSHA512 sha512 = BS.take 32 $ Serialize.encode sha512
