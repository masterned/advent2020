module Data.Vector where

import Prelude

newtype Vector2D
  = Vector2D { x :: Int, y :: Int }

instance showVector2D :: Show Vector2D where
  show :: Vector2D -> String
  show (Vector2D { x, y }) = "Vector [" <> show x <> ", " <> show y <> "]"

derive instance eqVector2D :: Eq Vector2D

derive instance ordVector2D :: Ord Vector2D

instance semiringVector2D :: Semiring Vector2D where
  add :: Vector2D -> Vector2D -> Vector2D
  add (Vector2D { x: x1, y: y1 }) (Vector2D { x: x2, y: y2 }) = Vector2D { x: x1 + x2, y: y1 + y2 }
  zero :: Vector2D
  zero = Vector2D { x: zero, y: zero }
  mul :: Vector2D -> Vector2D -> Vector2D
  mul (Vector2D { x: x1, y: y1 }) (Vector2D { x: x2, y: y2 }) = Vector2D { x: x1 * x2, y: y1 * y2 }
  one :: Vector2D
  one = Vector2D { x: one, y: one }

instance ringVector2D :: Ring Vector2D where
  sub :: Vector2D -> Vector2D -> Vector2D
  sub (Vector2D { x: x1, y: y1 }) (Vector2D { x: x2, y: y2 }) = Vector2D { x: x1 - x2, y: y1 - y2 }

instance commutativeRingVector2D :: CommutativeRing Vector2D

vec2 :: Int -> Int -> Vector2D
vec2 x y = Vector2D { x, y }

getX :: Vector2D -> Int
getX (Vector2D { x }) = x

getY :: Vector2D -> Int
getY (Vector2D { y }) = y

bimap :: (Int -> Int) -> (Int -> Int) -> Vector2D -> Vector2D
bimap xFn yFn (Vector2D { x, y }) = Vector2D { x: xFn x, y: yFn y }

lmap :: (Int -> Int) -> Vector2D -> Vector2D
lmap xFn = bimap xFn identity

rmap :: (Int -> Int) -> Vector2D -> Vector2D
rmap yFn = bimap identity yFn
