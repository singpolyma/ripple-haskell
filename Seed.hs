module Seed (getSecret) where

import Data.Word
import Data.Base58Address (RippleAddress, rippleAddressPayload)
import Crypto.Hash.CryptoAPI (SHA512, hash')
import qualified Data.ByteString as BS
import qualified Data.Serialize as Serialize

import Crypto.Types.PubKey.ECDSA (PrivateKey(..))
import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), getCurveByName, CurveName(SEC_p256k1), Point)
import Codec.Crypto.ECC.Base (pmul)

import Crypto.Util (bs2i)

import IntegerBytes
import Hecc

-- g is the base point, n is the order thereof
g :: Point
n :: Integer
p256k1 :: Curve
p256k1@(CurveFP (CurvePrime _ (CurveCommon {ecc_g = g, ecc_n = n}))) = getCurveByName SEC_p256k1

-- | Derive the secret key for the given secret seed and address
getSecret ::
	RippleAddress    -- ^ Secret seed address
	-> RippleAddress -- ^ Public address
	-> PrivateKey
getSecret seed _ = PrivateKey p256k1 d
	where
	d = (sec + priv) `mod` n
	sec = bs2i $ gen (pub `BS.append` seq)
	pub = BS.pack $ toBytesCompressed $ pmul (point2hecc p256k1 g) priv
	priv = bs2i $ gen sbytes
	sbytes = BS.pack $ unroll (rippleAddressPayload seed)
	seq = Serialize.encode (0 :: Word32)

gen :: BS.ByteString -> BS.ByteString
gen bytes = fst $ until (\(x,_) -> n >= bs2i x) (\(_,i) -> (go i,i+1)) (go 0, 1)
	where
	go i = halfOfSHA512 $ hash' (bytes `BS.append` Serialize.encode (i :: Word32))

halfOfSHA512 :: SHA512 -> BS.ByteString
halfOfSHA512 sha512 = BS.take 32 $ Serialize.encode sha512
