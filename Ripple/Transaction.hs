module Ripple.Transaction (
	Transaction(..),
	TransactionType(..),
	Field(..)
) where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Word
import Data.LargeWord
import Data.Bits
import Data.Binary (Binary(..), Put, Get, putWord8, getWord8, encode)
import Data.Binary.Get (isEmpty, getLazyByteString)
import Data.Binary.Put (putLazyByteString)
import Data.Bool.HT (select)
import Crypto.Hash.CryptoAPI (SHA512, Hash(..), hash)
import Data.Base58Address (RippleAddress)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Serialize as Serialize

import Ripple.Amount

data TransactionType =
	Payment | AccountSet | SetRegularKey | OfferCreate | OfferCancel |
	Sign | TrustSet | OtherTransaction Word16
	deriving (Show, Eq)

instance Enum TransactionType where
	toEnum 00 = Payment
	toEnum 03 = AccountSet
	toEnum 05 = SetRegularKey
	toEnum 07 = OfferCreate
	toEnum 08 = OfferCancel
	toEnum 09 = Sign
	toEnum 20 = TrustSet
	toEnum x = OtherTransaction $ toEnum x

	fromEnum Payment       = 00
	fromEnum AccountSet    = 03
	fromEnum SetRegularKey = 05
	fromEnum OfferCreate   = 07
	fromEnum OfferCancel   = 08
	fromEnum Sign	  = 09
	fromEnum TrustSet      = 20
	fromEnum (OtherTransaction x) = fromEnum x

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

data TypedField =
	TF1  Word16             |
	TF2  Word32             |
	TF3  Word64             |
	TF4  Word128            |
	TF5  Word256            |
	TF6  Amount             |
	TF7  LZ.ByteString      |
	TF8  RippleAddress      |
	TF14 [TypedField]       |
	TF15 [[TypedField]]     |
	TF16 Word8              |
	TF17 Word160            |
	TF19 [Word256]
	deriving (Show, Eq)

putTF :: TypedField -> (Word8, Put)
putTF (TF1  x) = (01, put x)
putTF (TF2  x) = (02, put x)
putTF (TF3  x) = (03, put x)
putTF (TF4  x) = (04, put x)
putTF (TF5  x) = (05, put x)
putTF (TF6  x) = (05, put x)
putTF (TF7  x) = (07, put $ VariableLengthData x)
putTF (TF8  x) = (08, putWord8 20 >> put x)
putTF (TF16 x) = (16, put x)
putTF (TF17 x) = (17, put x)
putTF (TF19 x) = (19, put $ VariableLengthData $ LZ.concat (map encode x))

getTF :: Word8 -> Get TypedField
getTF 01 = TF1  <$> get
getTF 02 = TF2  <$> get
getTF 03 = TF3  <$> get
getTF 04 = TF4  <$> get
getTF 05 = TF5  <$> get
getTF 06 = TF6  <$> get
getTF 07 = (\(VariableLengthData x) -> TF7 x) <$> get
getTF 08 = TF8  <$> do
	len <- getWord8
	when (len /= 20) $
		fail $ "RippleAddress is 160 bit encoding, len is " ++ show len
	get
getTF 16 = TF16 <$> get
getTF 17 = TF17 <$> get
getTF x  = error $ "Unknown type for TypedField: " ++ show x

data Field =
	LedgerEntryType Word16          |
	TransactionType TransactionType |
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
	PublicKey LZ.ByteString         |
	MessageKey LZ.ByteString        |
	SigningPublicKey LZ.ByteString  |
	TransactionSignature LZ.ByteString |
	Generator LZ.ByteString         |
	Signature LZ.ByteString         |
	Domain LZ.ByteString            |
	FundScript LZ.ByteString        |
	RemoveScript LZ.ByteString      |
	ExpireScript LZ.ByteString      |
	CreateScript LZ.ByteString      |
	LedgerCloseTimeResolution Word8 |
	Account RippleAddress           |
	Owner RippleAddress             |
	Destination RippleAddress       |
	Issuer RippleAddress            |
	Target RippleAddress            |
	AuthorizedKey RippleAddress     |
	TemplateEntryType Word8         |
	TransactionResult Word8         |
	UnknownField Word8 TypedField
	deriving (Show, Eq)

instance Ord Field where
	compare x y = compare (tagPair x) (tagPair y)
		where
		tagPair f = let (tag, tf) = ungetField f in
			(fst $ putTF tf, tag)

instance Binary Field where
	get = do
		tag <- getWord8
		typ <- case tag `shiftR` 4 of
			0 -> get
			t -> return t
		fld <- case tag .&. 0x0F of
			0 -> get
			t -> return t
		tf <- getTF typ
		return $ getField fld tf

	put fld = mapM_ put header >> dta
		where
		header
			| typ < 16 && tag < 16 = [(typ `shiftL` 4) .|. tag]
			| typ < 16 = [typ `shiftL` 4, tag]
			| tag < 16 = [tag, typ]
			| otherwise = [0, typ, tag]
		(typ, dta) = putTF tf
		(tag, tf)  = ungetField fld

getField :: Word8 -> TypedField -> Field
getField 01 (TF1  x) = LedgerEntryType x
getField 02 (TF1  x) = TransactionType $ toEnum $ fromEnum x
getField 02 (TF2  x) = Flags x
getField 03 (TF2  x) = SourceTag x
getField 04 (TF2  x) = SequenceNumber x
getField 05 (TF2  x) = PreviousTransactionLedgerSequence x
getField 06 (TF2  x) = LedgerSequence x
getField 07 (TF2  x) = LedgerCloseTime x
getField 08 (TF2  x) = ParentLedgerCloseTime x
getField 09 (TF2  x) = SigningTime x
getField 10 (TF2  x) = ExpirationTime x
getField 11 (TF2  x) = TransferRate x
getField 12 (TF2  x) = WalletSize x
getField 01 (TF6  x) = Ripple.Transaction.Amount x
getField 02 (TF6  x) = Balance x
getField 03 (TF6  x) = Limit x
getField 04 (TF6  x) = TakerPays x
getField 05 (TF6  x) = TakerGets x
getField 06 (TF6  x) = LowLimit x
getField 07 (TF6  x) = HighLimit x
getField 08 (TF6  x) = Fee x
getField 09 (TF6  x) = SendMaximum x
getField 01 (TF7  x) = PublicKey x
getField 02 (TF7  x) = MessageKey x
getField 03 (TF7  x) = SigningPublicKey x
getField 04 (TF7  x) = TransactionSignature x
getField 05 (TF7  x) = Generator x
getField 06 (TF7  x) = Signature x
getField 07 (TF7  x) = Domain x
getField 08 (TF7  x) = FundScript x
getField 09 (TF7  x) = RemoveScript x
getField 10 (TF7  x) = ExpireScript x
getField 11 (TF7  x) = CreateScript x
getField 01 (TF8  x) = Account x
getField 02 (TF8  x) = Owner x
getField 03 (TF8  x) = Destination x
getField 04 (TF8  x) = Issuer x
getField 05 (TF8  x) = Target x
getField 06 (TF8  x) = AuthorizedKey x
getField 01 (TF16 x) = LedgerCloseTimeResolution x
getField 02 (TF16 x) = TemplateEntryType x
getField 03 (TF16 x) = TransactionResult x
getField tag tf      = UnknownField tag tf

ungetField :: Field -> (Word8, TypedField)
ungetField (LedgerEntryType x)           = (01, TF1 x)
ungetField (TransactionType x)           = (02, TF1 $ toEnum $ fromEnum x)
ungetField (Flags x)                     = (02, TF2 x)
ungetField (SourceTag x)                 = (03, TF2 x)
ungetField (SequenceNumber x)            = (04, TF2 x)
ungetField (PreviousTransactionLedgerSequence x) = (05, TF2 x)
ungetField (LedgerSequence x)            = (06, TF2 x)
ungetField (LedgerCloseTime x)           = (07, TF2 x)
ungetField (ParentLedgerCloseTime x)     = (08, TF2 x)
ungetField (SigningTime x)               = (09, TF2 x)
ungetField (ExpirationTime x)            = (10, TF2 x)
ungetField (TransferRate x)              = (11, TF2 x)
ungetField (WalletSize x)                = (12, TF2 x)
ungetField (Ripple.Transaction.Amount x) = (01, TF6 x)
ungetField (Balance x)                   = (02, TF6 x)
ungetField (Limit x)                     = (03, TF6 x)
ungetField (TakerPays x)                 = (04, TF6 x)
ungetField (TakerGets x)                 = (05, TF6 x)
ungetField (LowLimit x)                  = (06, TF6 x)
ungetField (HighLimit x)                 = (07, TF6 x)
ungetField (Fee x)                       = (08, TF6 x)
ungetField (SendMaximum x)               = (09, TF6 x)
ungetField (PublicKey x)                 = (01, TF7 x)
ungetField (MessageKey x)                = (02, TF7 x)
ungetField (SigningPublicKey x)          = (03, TF7 x)
ungetField (TransactionSignature x)      = (04, TF7 x)
ungetField (Generator x)                 = (05, TF7 x)
ungetField (Signature x)                 = (06, TF7 x)
ungetField (Domain x)                    = (07, TF7 x)
ungetField (FundScript x)                = (08, TF7 x)
ungetField (RemoveScript x)              = (09, TF7 x)
ungetField (ExpireScript x)              = (10, TF7 x)
ungetField (CreateScript x)              = (11, TF7 x)
ungetField (Account x)                   = (01, TF8 x)
ungetField (Owner x)                     = (02, TF8 x)
ungetField (Destination x)               = (03, TF8 x)
ungetField (Issuer x)                    = (04, TF8 x)
ungetField (Target x)                    = (05, TF8 x)
ungetField (AuthorizedKey x)             = (06, TF8 x)
ungetField (LedgerCloseTimeResolution x) = (01, TF16 x)
ungetField (TemplateEntryType x)         = (02, TF16 x)
ungetField (TransactionResult x)         = (03, TF16 x)
ungetField (UnknownField tag tf)         = (tag, tf)

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
