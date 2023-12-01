module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Person (Person)
import Data.Picture (Shape(..), getCenter, origin)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k     = 0
             | otherwise = factorial n / (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n-1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } | c1 == c2  = true
                                                             | otherwise = false

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [b] = b
fromSingleton a _ = a

circleAtOrigin :: Shape 
circleAtOrigin = Circle origin 10.0

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ w h) = Rectangle origin w h
centerShape line@(Line s e) = Line (s - delta) (e - delta)
  where
  delta = getCenter line
centerShape (Text _ t) = Text origin t
centerShape (Clipped p _ w h) = Clipped p origin w h

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (r * i)
scaleShape i (Rectangle c w h) = Rectangle c (w * i) (h * i)
scaleShape i (Line s e) = Line (s * scale) (e * scale)
  where
  scale = {x: i, y: i}
scaleShape _ text = text

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp i) (Volt v) = Watt $ i * v

area :: Shape -> Number
area (Circle _ r) = Number.pi * r * r
area (Rectangle _ h w) = h * w
area _ = 0.0
