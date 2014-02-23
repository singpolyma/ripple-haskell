module Ripple.Sign (signTransaction) where

import Data.Word (Word32)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Binary (encode)
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LZ

import Crypto.Hash.CryptoAPI (SHA512, Hash(..), hash)
import Crypto.Types.PubKey.ECDSA (PrivateKey(..))
import Crypto.Random (CryptoRandomGen, GenError)
import ECDSA (sign, publicToBytes, publicFromPrivate, signatureEncodeDER)

import Ripple.Transaction

signTransaction :: (CryptoRandomGen g) =>
	Transaction -> PrivateKey -> g -> Either GenError (Transaction, g)
signTransaction (Transaction fs) private g =
	first (addSig . signatureEncodeDER) <$> sign private hash g
	where
	addSig s = Transaction ((TransactionSignature $ LZ.fromChunks [s]) : fs')
	hash = signing_hash (Transaction fs')
	fs' = SigningPublicKey publicKey : filter (not.isSPK) fs
	publicKey = LZ.fromChunks [publicToBytes $ publicFromPrivate private]

isSPK :: Field -> Bool
isSPK (SigningPublicKey {}) = True
isSPK _ = False

hash_sign :: Word32
hash_sign = 0x53545800

compute_hash :: (Hash ctx d) => Transaction -> d
compute_hash t = hash (encode hash_sign `LZ.append` encode t)

signing_hash :: Transaction -> BS.ByteString
signing_hash t = BS.take 32 $ Serialize.encode sha512
	where
	sha512 = compute_hash t :: SHA512
