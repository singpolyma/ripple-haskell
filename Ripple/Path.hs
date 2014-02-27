module Ripple.Path (PathSet(..), Path(..), PathEntry(..)) where

import Data.Bits ((.&.), (.|.))
import Data.List (find)
import Control.Arrow (first, second)
import Control.Applicative ((<$>), (<*>))
import Data.Word (Word8)
import Data.Binary (Binary(..), Get, putWord8, getWord8)
import Data.Binary.Get (lookAheadM)
import Data.Base58Address (RippleAddress)

import Ripple.Amount (CurrencySpecifier)

newtype PathSet = PathSet [Path]
	deriving (Show, Eq)

instance Binary PathSet where
	get = do
		maybeEmpty <- lookAheadM (maybeGetWord8 [0x00])
		case maybeEmpty of
			Just _ -> return (PathSet [Path []])
			_      -> PathSet <$> getPaths

	put (PathSet []) = fail "Empty PathSet is not allowed"
	put (PathSet (Path entries : [])) = do
		mapM_ put entries
		putWord8 0x00
	put (PathSet (Path entries : ps)) = do
		mapM_ put entries
		putWord8 0xFF
		put (PathSet ps)

getPaths :: Get [Path]
getPaths = do
	(done, path) <- getEntries
	if done then return [Path path] else
		getPaths >>= \rest -> return (Path path : rest)

getEntries :: Get (Bool, [PathEntry])
getEntries = do
	entry <- get
	nextOrDone <- lookAheadM (maybeGetWord8 [0xFF,0x00])
	case nextOrDone of
		Just 0x00 -> return (True, [entry])
		Just 0xFF -> return (False, [entry])
		_         -> getEntries >>= \(done, rest) -> return (done, entry:rest)

newtype Path = Path [PathEntry]
	deriving (Show, Eq)

data PathEntry = PathEntry {
		account    :: Maybe RippleAddress,
		toCurrency :: Maybe CurrencySpecifier,
		issuer     :: Maybe RippleAddress
	} deriving (Show, Eq)

instance Binary PathEntry where
	get = getWord8 >>= \typ -> PathEntry <$>
		(if typ .&. 0x01 == 0x01 then Just <$> get else return Nothing) <*>
		(if typ .&. 0x10 == 0x10 then Just <$> get else return Nothing) <*>
		(if typ .&. 0x20 == 0x20 then Just <$> get else return Nothing)

	put (PathEntry Nothing Nothing Nothing) =
		fail "Invalid empty PathEntry"
	put (PathEntry account currency issuer) = putWord8 typ >> dta
		where
		(dta, typ) = first sequence_ $ second (foldr (.|.) 0x00) $ unzip [
				maybePut 0x01 account,
				maybePut 0x10 currency,
				maybePut 0x20 issuer
			]
		maybePut tag = maybe (return (), 0x00) (\x -> (put x, tag))

maybeGetWord8 :: [Word8] -> Get (Maybe Word8)
maybeGetWord8 ws = fmap (\w -> find (==w) ws) getWord8
