-----------------------------------------------------------------------------
-- |
-- Module      :  Data.WM
-- Copyright   :  (c) Ben Boeckel 2013
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  mathstuf@gmail.com
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--

module Data.WM (
      WM(..)
    ) where

import Data.WM.Window
import Data.WM.Workspace
import Data.WM.Screen

data WM t l w s ctx = WM {
      current :: !(Screen t l w s ctx)
    , visible :: [Screen t l w s ctx]
    , hidden  :: [Workspace t l w]
    } deriving (Show, Read, Eq)
