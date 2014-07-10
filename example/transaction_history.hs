{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Prelude (show,putStrLn)
import BasicPrelude hiding (show,putStrLn)
import Numeric (showHex)
import Network (withSocketsDo)
import Control.Monad.Loops (allM)
import Data.Time.LocalTime (LocalTime, zonedTimeToLocalTime, utcToLocalZonedTime)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Error (note, fmapL, EitherT(..), runEitherT)
import Data.Base58Address (RippleAddress)
import Text.Printf (printf)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Ripple.Transaction
import Ripple.WebSockets
import qualified Ripple.Amount as A

-- This is a hack to make it work with readArgs
deriving instance Typeable RippleAddress

main :: IO ()
main = withSocketsDo $ do
	(addr, startTime, endTime, out) <- readArgs
	x <- WS.runClient "s_east.ripple.com" 443 "/" $ \conn -> runEitherT $ do
		sendJSON conn CommandLedgerClosed
		ResultLedgerClosed _ idx <- EitherT (getRippleResult <$> receiveJSON conn)

		diff <- (`diffUTCTime` startTime) . result_close_time <$> fetchLedger conn idx
		let realisticMax = idx - (floor diff `div` 30)
		let realisticMin = max 40000 (idx - floor diff)

		firstLedger <- findLedgerAt conn startTime realisticMin realisticMax
			((realisticMin + realisticMax) `div` 2)

		liftIO $ writeFile out $ T.pack "Date,TransactionHash,Account,Amount,Currency,DestinationTag,InvoiceID\n"
		byPages conn addr firstLedger idx $ \ledger (Transaction fields) -> do
			let fs = Set.fromList fields
			let headers = (,,,) <$>
				Set.lookupLE (TransactionType {}) fs <*>
				Set.lookupLE (Destination {}) fs <*>
				Set.lookupLE (TransactionResult {}) fs <*>
				Set.lookupLE (SigningTime {}) fs
			case headers of
				Just (_,_,_,SigningTime t)
					| fromRippleTime t > endTime -> return False
				Just (TransactionType Payment, Destination dst, TransactionResult 0, SigningTime t)
					| dst == addr -> do
						t' <- zonedTimeToLocalTime <$> utcToLocalZonedTime (fromRippleTime t)
						let line = printLine t'
							(Set.lookupLE (TransactionHash {}) fs)
							(Set.lookupLE (Account {}) fs)
							(getAmount
								(Set.lookupLE (DeliveredAmount {}) fs)
								(Set.lookupLE (Amount {}) fs)
							)
							(Set.lookupLE (DestinationTag {}) fs)
							(Set.lookupLE (InvoiceID {}) fs)
						when (not $ null line) $ appendFile out (T.pack $ line ++ "\n")
						return True -- keep going
				_ -> return True -- skip

	case x of
		Left e -> print e
		Right _ -> return ()

printLine :: LocalTime -> Maybe Field -> Maybe Field -> Maybe (Double, String) -> Maybe Field -> Maybe Field -> String
printLine t (Just (TransactionHash h)) (Just (Account acc)) (Just (amount, currency)) dt inv =
	intercalate "," $ [show t,hex h,show acc,printf "%f" amount,currency,dtS dt,invS inv]
	where
	dtS (Just (DestinationTag dt)) = show dt
	dtS _ = ""

	invS (Just (InvoiceID inv)) = hex inv
	invS _ = ""
printLine _ _ _ _ _ _ = ""

hex :: (Integral a, Show a) => a -> String
hex i = showHex (toInteger i) ""

fromRippleTime :: Word32 -> UTCTime
fromRippleTime = posixSecondsToUTCTime . fromInteger . (+946684800) . toInteger

getAmount :: Maybe Field -> Maybe Field -> Maybe (Double, String)
getAmount (Just (DeliveredAmount amount)) _ = case amount of
	A.Amount rat (A.Currency (x,y,z) _) -> Just (realToFrac rat,[x,y,z])
	A.Amount rat A.XRP -> Just (realToFrac rat,"XRP")
getAmount _ amnt@(Just (Amount amount)) =
	getAmount (Just (DeliveredAmount amount)) amnt
getAmount _ _ = Nothing

findLedgerAt :: (MonadIO m, Functor m) => WS.Connection -> UTCTime -> Integer -> Integer -> Integer -> EitherT RippleError m Integer
findLedgerAt conn time end1 end2 idx = do
	r <- fetchLedger conn idx
	let diff = diffUTCTime time (result_close_time r)
	if (diff >= 0 && diff <= 10) || (end2 - end1) < 2 then
			return $ result_ledger_index r
		else if diff > 0 then -- after this
			findLedgerAt conn time idx end2 ((idx + end2) `div` 2)
		else
			findLedgerAt conn time end1 idx ((end1 + idx) `div` 2)

fetchLedger :: (MonadIO m, Functor m) => WS.Connection -> Integer -> EitherT RippleError m ResultLedger
fetchLedger conn idx = do
	sendJSON conn (CommandLedger (Just idx) False False)
	EitherT (getRippleResult <$> receiveJSON conn)

byPages :: WS.Connection -> RippleAddress -> Integer -> Integer -> (Integer -> Transaction -> IO Bool) -> EitherT RippleError IO ()
byPages conn addr startLedger endLedger k = go 0
	where
	go offset = do
		sendJSON conn (CommandAccountTX addr pageSize (Just offset)
			(Just (startLedger-1)) (Just (endLedger+1)) False False)
		ResultAccountTX ts <- EitherT (getRippleResult <$> receiveJSON conn)

		case ts of
			[] -> return ()
			_ | any ((==endLedger).fst) ts ->
				liftIO $ mapM_ (uncurry k) $ filter ((/=endLedger).fst) ts
			  | otherwise -> do
				continue <- liftIO $ allM (uncurry k) ts
				when continue $ go (offset + pageSize)

pageSize :: Int
pageSize = 100
