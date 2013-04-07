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

module Data.WM
    ( WM(..)
    , module Data.WM.Screen
    , module Data.WM.Window
    , module Data.WM.Workspace
    ) where

import Data.WM.Window
import Data.WM.Workspace
import Data.WM.Screen

data WM t l w s ctx = WM
    { wmCurrent :: !(Screen t l w s ctx)
    , wmVisible :: [Screen t l w s ctx]
    , wmHidden  :: [Workspace t l s]
    } deriving (Show, Read, Eq)
