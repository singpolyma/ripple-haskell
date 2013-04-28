module Seed (getSecret) where

import Data.Word
import Data.Base58Address (RippleAddress, rippleAddressPayload)
import Data.Binary (encode)
import Crypto.Hash.CryptoAPI (SHA512, Hash(..), hash)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Serialize as Serialize

import Crypto.Types.PubKey.ECDSA (PrivateKey(..))
import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), Point(..), getCurveByName, CurveName(SEC_p256k1))

import Codec.Crypto.ECC.Base -- hecc

import Data.Sequence (unfoldl)
import Data.Foldable (toList)

-- g is the base point, n is the order thereof
p256k1@(CurveFP (CurvePrime _ (CurveCommon {ecc_g = g, ecc_n = n}))) = getCurveByName SEC_p256k1

-- | Derive the secret key for the given secret seed and address
getSecret ::
	RippleAddress    -- ^ Secret seed address
	-> RippleAddress -- ^ Public address
	-> PrivateKey
getSecret seed address = PrivateKey p256k1 d
	where
	d = (sec + priv) `mod` n
	sec = roll' $ gen (pub `LZ.append` seq)
	pub = LZ.pack $ toBytesCompressed $ pmul (point2hecc p256k1 g) priv
	priv = roll' $ gen sbytes
	sbytes = LZ.pack $ unroll (rippleAddressPayload seed)
	seq = encode (0 :: Word32)

gen bytes = fst $ until (\(x,_) -> n >= roll' x) (\(_,i) -> (go i,i+1)) (go 0, 1)
	where
	go i = halfOfSHA512 $ hash (bytes `LZ.append` encode (i :: Word32))

halfOfSHA512 :: SHA512 -> LZ.ByteString
halfOfSHA512 sha512 = LZ.take 32 $ Serialize.encodeLazy sha512

-- XXX: the following is too verbose

unroll :: Integer -> [Word8]
unroll = toBase 256

roll :: [Word8] -> Integer
roll = fromBase 256

roll' :: LZ.ByteString -> Integer
roll' = roll . LZ.unpack

toBase :: (Integral a, Integral b) => a -> a -> [b]
toBase _ 0 = [0]
toBase b v
	| v < 0 = error "toBase v < 0"
	| otherwise = map fromIntegral $ toList $
		unfoldl (\n -> if n == 0 then Nothing else Just $! (n `divMod` b)) v

fromBase :: (Integral a, Integral b) => b -> [a] -> b
fromBase b = foldl (\n k -> n * b + fromIntegral k) 0

-- Adapters for hecc

curve2hecc :: Curve -> EC Integer
curve2hecc (CurveFP (CurvePrime p (CurveCommon a b g n 1))) =
	ECi (256, a, b, p, n) -- HACK, fixed to 256 bits

point2hecc :: Curve -> Point -> ECPF Integer
point2hecc curve (Point x y) =
	ECPa (curve2hecc curve, x, y)

toBytesCompressed :: ECPF Integer -> [Word8]
toBytesCompressed point =
	(if gety point `mod` 2 == 0 then 0x02 else 0x03) :
	unroll (getx point)
