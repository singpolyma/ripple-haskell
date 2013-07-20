module Websocket (rippleWS, RippleError) where

import Control.Applicative ((<$>))
import Control.Monad (void, forever, when)
import Control.Monad.Trans (liftIO)
import System.IO.Error (ioError, userError)
import Control.Proxy.Concurrent (Input, Output, spawn, recv, send, Buffer(Unbounded), forkIO, atomically)
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson

-- TODO extract error message
data RippleError = RippleError
	deriving (Show, Eq)

newtype Result a = Result (Either RippleError a)

instance (Aeson.FromJSON a) => Aeson.FromJSON (Result a) where
	parseJSON (Aeson.Object o) = Result <$> do
		status <- o .: T.pack "status"
		result <- o .: T.pack "result"
		case (status, result) of
			("success", Aeson.Object o') ->
				fmap Right (Aeson.parseJSON (Aeson.Object o'))
			_ -> fail "Invalid Ripple Result"
	parseJSON _ = fail "Ripple Result is alsways a JSON object"

wsReceiveJSON :: (WS.TextProtocol p, Aeson.FromJSON j) => WS.WebSockets p (Either RippleError j)
wsReceiveJSON = do
	x <- WS.receiveData
	case Aeson.decode x of
		Just (Result v) -> return v
		Nothing -> return (Left RippleError)

rippleWS' :: (Aeson.ToJSON i, Aeson.FromJSON o) => Output i -> Input (Either RippleError o) -> WS.WebSockets WS.Hybi10 ()
rippleWS' inputOut outputIn = do
	sink <- WS.getSink
	liftIO $ void $ forkIO $ sendLoop sink
	receiveLoop
	where
	sendLoop sink = do
		i <- atomically (recv inputOut)
		case i of
			Just x -> do
				WS.sendSink sink $ WS.textData $ Aeson.encode x
				sendLoop sink
			Nothing -> return ()

	receiveLoop = do
		result <- wsReceiveJSON >>= liftIO . atomically . send outputIn
		when result receiveLoop

-- | If the connection terminates, the mailboxes will start to error on use
rippleWS :: (Aeson.ToJSON i, Aeson.FromJSON o) => String -> Int -> String -> IO (Input i, Output (Either RippleError o))
rippleWS host port path = do
	(inputIn, inputOut) <- spawn Unbounded
	(outputIn, outputOut) <- spawn Unbounded
	void $ forkIO $ WS.connect host port path (rippleWS' inputOut outputIn)
	return (inputIn, outputOut)
