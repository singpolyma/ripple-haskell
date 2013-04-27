{-# LANGUAGE CPP #-}
module Binary where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Word
import Data.Bits
import Data.Binary (Binary(..), Get, putWord8, getWord8, encode)
import Data.Binary.Get (isEmpty, getLazyByteString)
import Data.Binary.Put (putLazyByteString)
import Data.Bool.HT (select)
import Crypto.Hash.CryptoAPI (SHA512, Hash(..), hash)
import Data.Base58Address (RippleAddress)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Serialize as Serialize

import Amount
#include "Derive.hs"

newtype VariableLengthData = VariableLengthData LZ.ByteString
	deriving (Show, Eq)

instance Binary VariableLengthData where
	get = do
		tag <- getWord8
		len <- select (fail "could not determine length of VariableLengthData") [
				(tag < 193, return $ fromIntegral tag),
				(tag < 241, do
						tag2 <- getWord8
						return $
							193 + ((fromIntegral tag - 193)*256) +
							fromIntegral tag2
					),
				(tag < 255, do
						(tag2, tag3) <- (,) <$> getWord8 <*> getWord8
						return $
							12481 + ((fromIntegral tag - 241)*65536) +
							(fromIntegral tag2 * 256) + fromIntegral tag3
					)
			]
		VariableLengthData <$> getLazyByteString len

	put (VariableLengthData bytes) =
		mapM_ (putWord8.fromIntegral) tag >> putLazyByteString bytes
		where
		tag
			| l < 193 = [l]
			| l < 16320 = [(l2 `div` 256) + 193, l2 `mod` 256]
			| l < 995520 = [(l3 `div` 65536) + 241, (l3 `mod` 65536) `div` 256, (l3 `mod` 65536) `mod` 256]
			| otherwise = error "Data too long for VariableLengthData"
		l3 = l - 12481
		l2 = l - 193
		l = LZ.length bytes

data Field =
	LedgerEntryType Word16          |
	TransactionType Word16          |
	Flags Word32                    |
	SourceTag Word32                |
	SequenceNumber Word32           |
	PreviousTransactionLedgerSequence Word32 |
	LedgerSequence Word32           |
	LedgerCloseTime Word32          |
	ParentLedgerCloseTime Word32    |
	SigningTime Word32              |
	ExpirationTime Word32           |
	TransferRate Word32             |
	WalletSize Word32               |
	Amount Amount                   |
	Balance Amount                  |
	Limit Amount                    |
	TakerPays Amount                |
	TakerGets Amount                |
	LowLimit Amount                 |
	HighLimit Amount                |
	Fee Amount                      |
	SendMaximum Amount              |
	PublicKey VariableLengthData    |
	MessageKey VariableLengthData   |
	SigningPublicKey VariableLengthData |
	TransactionSignature VariableLengthData |
	Generator VariableLengthData    |
	Signature VariableLengthData    |
	Domain VariableLengthData       |
	FundScript VariableLengthData   |
	RemoveScript VariableLengthData |
	ExpireScript VariableLengthData |
	CreateScript VariableLengthData |
	LedgerCloseTimeResolution Word8 |
	Account RippleAddress           |
	Owner RippleAddress             |
	Destination RippleAddress       |
	Issuer RippleAddress            |
	Target RippleAddress            |
	AuthorizedKey RippleAddress     |
	TemplateEntryType Word8         |
	TransactionResult Word8
	deriving (Show, Eq)

instance Ord Field where
	compare x y = compare (tagField x) (tagField y)

instance Binary Field where
	get = do
		tag <- getWord8
		typ <- case tag `shiftR` 4 of
			0 -> get
			t -> return t
		fld <- case tag .&. 0x0F of
			0 -> get
			t -> return t
		getField (typ,fld)

	put (LedgerEntryType x) = mapM_ put (packTypFld (01, 01)) >> put x
	put (TransactionType x) = mapM_ put (packTypFld (01, 02)) >> put x
	put (Flags x) = mapM_ put (packTypFld (02, 02)) >> put x
	put (SourceTag x) = mapM_ put (packTypFld (02, 03)) >> put x
	put (SequenceNumber x) = mapM_ put (packTypFld (02, 04)) >> put x
	put (PreviousTransactionLedgerSequence x) = mapM_ put (packTypFld (02, 05)) >> put x
	put (LedgerSequence x) = mapM_ put (packTypFld (02, 06)) >> put x
	put (LedgerCloseTime x) = mapM_ put (packTypFld (02, 07)) >> put x
	put (ParentLedgerCloseTime x) = mapM_ put (packTypFld (02, 08)) >> put x
	put (SigningTime x) = mapM_ put (packTypFld (02, 09)) >> put x
	put (ExpirationTime x) = mapM_ put (packTypFld (02, 10)) >> put x
	put (TransferRate x) = mapM_ put (packTypFld (02, 11)) >> put x
	put (Binary.Amount x) = mapM_ put (packTypFld (06, 01)) >> put x
	put (Balance x) = mapM_ put (packTypFld (06, 02)) >> put x
	put (Limit x) = mapM_ put (packTypFld (06, 03)) >> put x
	put (TakerPays x) = mapM_ put (packTypFld (06, 04)) >> put x
	put (TakerGets x) = mapM_ put (packTypFld (06, 05)) >> put x
	put (LowLimit x) = mapM_ put (packTypFld (06, 06)) >> put x
	put (HighLimit x) = mapM_ put (packTypFld (06, 07)) >> put x
	put (Fee x) = mapM_ put (packTypFld (06, 08)) >> put x
	put (SendMaximum x) = mapM_ put (packTypFld (06, 09)) >> put x
	put (PublicKey x) = mapM_ put (packTypFld (07, 01)) >> put x
	put (MessageKey x) = mapM_ put (packTypFld (07, 02)) >> put x
	put (SigningPublicKey x) = mapM_ put (packTypFld (07, 03)) >> put x
	put (TransactionSignature x) = mapM_ put (packTypFld (07, 04)) >> put x
	put (Generator x) = mapM_ put (packTypFld (07, 05)) >> put x
	put (Signature x) = mapM_ put (packTypFld (07, 06)) >> put x
	put (Domain x) = mapM_ put (packTypFld (07, 07)) >> put x
	put (FundScript x) = mapM_ put (packTypFld (07, 08)) >> put x
	put (RemoveScript x) = mapM_ put (packTypFld (07, 09)) >> put x
	put (ExpireScript x) = mapM_ put (packTypFld (07, 10)) >> put x
	put (CreateScript x) = mapM_ put (packTypFld (07, 11)) >> put x
	put (Account x) = mapM_ put (packTypFld (08, 01)) >> putWord8 20 >> put x
	put (Owner x) = mapM_ put (packTypFld (08, 02)) >> putWord8 20 >> put x
	put (Destination x) = mapM_ put (packTypFld (08, 03)) >> putWord8 20 >> put x
	put (Issuer x) = mapM_ put (packTypFld (08, 04)) >> putWord8 20 >> put x
	put (Target x) = mapM_ put (packTypFld (08, 05)) >> putWord8 20 >> put x
	put (AuthorizedKey x) = mapM_ put (packTypFld (08, 06)) >> putWord8 20 >> put x
	put (LedgerCloseTimeResolution x) = mapM_ put (packTypFld (16, 01)) >> put x
	put (TemplateEntryType x) = mapM_ put (packTypFld (16, 02)) >> put x
	put (TransactionResult x) = mapM_ put (packTypFld (16, 03)) >> put x

getField :: (Word8,Word8) -> Get Field
getField (01,01) = LedgerEntryType <$> get
getField (01,02) = TransactionType <$> get
getField (02,02) = Flags <$> get
getField (02,03) = SourceTag <$> get
getField (02,04) = SequenceNumber <$> get
getField (02,05) = PreviousTransactionLedgerSequence <$> get
getField (02,06) = LedgerSequence <$> get
getField (02,07) = LedgerCloseTime <$> get
getField (02,08) = ParentLedgerCloseTime <$> get
getField (02,09) = SigningTime <$> get
getField (02,10) = ExpirationTime <$> get
getField (02,11) = TransferRate <$> get
getField (02,12) = WalletSize <$> get
getField (06,01) = Binary.Amount <$> get
getField (06,02) = Balance <$> get
getField (06,03) = Limit <$> get
getField (06,04) = TakerPays <$> get
getField (06,05) = TakerGets <$> get
getField (06,06) = LowLimit <$> get
getField (06,07) = HighLimit <$> get
getField (06,08) = Fee <$> get
getField (06,09) = SendMaximum <$> get
getField (07,01) = PublicKey <$> get
getField (07,02) = MessageKey <$> get
getField (07,03) = SigningPublicKey <$> get
getField (07,04) = TransactionSignature <$> get
getField (07,05) = Generator <$> get
getField (07,06) = Signature <$> get
getField (07,07) = Domain <$> get
getField (07,08) = FundScript <$> get
getField (07,09) = RemoveScript <$> get
getField (07,10) = ExpireScript <$> get
getField (07,11) = CreateScript <$> get
getField (08,01) = Account <$> getVariableRippleAddress
getField (08,02) = Owner <$> getVariableRippleAddress
getField (08,03) = Destination <$> getVariableRippleAddress
getField (08,04) = Issuer <$> getVariableRippleAddress
getField (08,05) = Target <$> getVariableRippleAddress
getField (08,06) = AuthorizedKey <$> getVariableRippleAddress
getField (16,01) = LedgerCloseTimeResolution <$> get
getField (16,02) = TemplateEntryType <$> get
getField (16,03) = TransactionResult <$> get
getField x = fail $ "Unknown Ripple field: " ++ show x

-- For weird encoding of address that also includes length
getVariableRippleAddress :: Get RippleAddress
getVariableRippleAddress = do
	len <- getWord8
	when (len /= 20) $
		fail $ "RippleAddress is 160 bit encoding, len is " ++ show len
	get

packTypFld :: (Word8,Word8) -> [Word8]
packTypFld (x,y)
	| x < 16 && y < 16 = [(x `shiftL` 4) .|. y]
	| x < 16 = [x `shiftL` 4, y]
	| y < 16 = [y, x]
	| otherwise = [0, x, y]

newtype Transaction = Transaction [Field]
	deriving (Show, Eq)

instance Binary Transaction where
	get = Transaction <$> listUntilEnd
	put (Transaction fs) = mapM_ put (sort fs)

listUntilEnd :: (Binary a) => Get [a]
listUntilEnd = do
	done <- isEmpty
	if done then return [] else do
		next <- get
		rest <- listUntilEnd
		return (next:rest)
{-# INLINE listUntilEnd #-}

hash_sign :: Word32
hash_sign = 0x53545800

compute_hash :: (Hash ctx d) => Transaction -> d
compute_hash t = hash (encode hash_sign `LZ.append` encode t)

signing_hash :: Transaction -> LZ.ByteString
signing_hash t = LZ.take 32 $ Serialize.encodeLazy sha512
	where
	sha512 = compute_hash t :: SHA512
