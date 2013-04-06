-----------------------------------------------------------------------------
-- |
-- Module      :  Data.WM.FloatStack
-- Copyright   :  (c) Ben Boeckel 2013
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  mathstuf@gmail.com
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--

module Data.WM.FloatStack (
      FloatStack
    ) where

import Data.WM.Window
import Data.WM.Workspace

data FloatStack w = FloatStack {
      fsFocused   :: !w
    , fsPrevious  :: [w]
    , fsNext      :: [w]
    } deriving (Show, Read, Eq)

instance WindowStack FloatStack where
    wsFocused  = fsFocused
    wsPrevious = fsPrevious
    wsNext     = fsNext
