module Websocket2 where

import Control.Applicative ((<$>))
import qualified Data.Text as T

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

data RippleError =
	UnknownCommand |
	OtherRippleError Int String String
	deriving (Show, Eq)

newtype RippleResult a = RippleResult (Either RippleError a)
	deriving (Show, Eq)

instance (Aeson.FromJSON a) => Aeson.FromJSON (RippleResult a) where
	parseJSON (Aeson.Object o) = RippleResult <$> do
		status <- o .: T.pack "status"
		typ <- o .: T.pack "type"
		case (status, typ) of
			("success", "response") ->
				Right <$> (Aeson.parseJSON =<< o.: T.pack "result")
			("error", "response") -> do
				err  <- o .: T.pack "error"
				code <- o .: T.pack "error_code"
				msg  <- o .: T.pack "error_message"
				case code of
					27 -> return $ Left $ UnknownCommand
					_ -> return $ Left $ OtherRippleError code err msg
			_ -> fail "Invalid Ripple Result"
	parseJSON _ = fail "Ripple Result is always a JSON object"

receiveJSON :: (Aeson.FromJSON j) => WS.Connection -> IO (Either String j)
receiveJSON = fmap Aeson.eitherDecode . WS.receiveData

sendJSON :: (Aeson.ToJSON j) => WS.Connection -> j -> IO ()
sendJSON conn = WS.sendTextData conn . Aeson.encode
