module Go where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LZ

import Crypto.Types.PubKey.ECDSA (PrivateKey(..))
import Crypto.Random (CryptoRandomGen, GenError)

import Binary
import qualified Amount
import ECDSA

-- Example transaction to send
send = Transaction [
		TransactionType 0,
		Account (read "rhKJE9kFPz6DuK4KyL2o8NkCCNPKnSQGRL"),
		SequenceNumber 185,
		Fee (Amount.Amount (10/1000000) Amount.XRP),
		-- SigningPublicKey
		-- TxnSignature
		Destination (read "r3ADD8kXSUKHd6zTCKfnKT3zV9EZHjzp1S"),
		Amount (Amount.Amount (1/1000000) Amount.XRP)
	]

signTransaction :: (CryptoRandomGen g) => Transaction -> PrivateKey -> g -> Either GenError (Transaction, g)
signTransaction (Transaction fs) private g = do
	(signature, g') <- sign private hash g
	let signatureBytes = toLazyBS $ signatureEncodeDER signature
	return (Transaction ((TransactionSignature $ VariableLengthData signatureBytes):fs'), g')
	where
	hash = toStrictBS $ signing_hash (Transaction fs')
	fs' = (SigningPublicKey $ VariableLengthData publicKey) : filter (not.isSPK) fs
	publicKey = toLazyBS $ publicToBytes $ publicFromPrivate private
	isSPK (SigningPublicKey {}) = True
	isSPK _ = False

toLazyBS :: BS.ByteString -> LZ.ByteString
toLazyBS = LZ.fromChunks . (:[])

toStrictBS :: LZ.ByteString -> BS.ByteString
toStrictBS = BS.concat . LZ.toChunks
