module Hecc where

import Data.Word

import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), Point(..))
import Codec.Crypto.ECC.Base -- hecc

import IntegerBytes (unroll)

-- Adapters for hecc

curve2hecc :: Curve -> EC Integer
curve2hecc (CurveFP (CurvePrime p (CurveCommon a b _ n 1))) =
	ECi (8 * length (unroll n), a, b, p, n)
curve2hecc _ = error "TODO: binary curves"

point2hecc :: Curve -> Point -> ECPF Integer
point2hecc curve (Point x y) =
	ECPa (curve2hecc curve, x, y)

hecc2point :: ECPF Integer -> Point
hecc2point p = Point (getx p) (gety p)

toBytesCompressed :: ECPF Integer -> [Word8]
toBytesCompressed point =
	(if gety point `mod` 2 == 0 then 0x02 else 0x03) :
	unroll (getx point)
