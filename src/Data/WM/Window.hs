-----------------------------------------------------------------------------
-- |
-- Module      :  Data.WM.Window
-- Copyright   :  (c) Ben Boeckel 2013
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  mathstuf@gmail.com
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--

module Data.WM.Window (
      RationalRect(..)
    , Window(..)
    ) where

data RationalRect = RationalRect {
      rectTop      :: !Rational
    , rectLeft     :: !Rational
    , rectWidth    :: !Rational
    , rectHeight   :: !Rational
    , rectRotation :: !Double
    } deriving (Show, Read, Eq)

class Window w where
    parent   :: w -> Maybe w
    children :: w -> [w]
    location :: w -> RationalRect
