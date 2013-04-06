-----------------------------------------------------------------------------
-- |
-- Module      :  Data.WM.Screen
-- Copyright   :  (c) Ben Boeckel 2013
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  mathstuf@gmail.com
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--

module Data.WM.Screen (
      Screen(..)
    ) where

import Data.WM.Workspace

data Screen t l w s ctx = Screen {
      workspace :: Workspace t l s
    , screen    :: !s
    , context   :: ctx
    } deriving (Show, Read, Eq)
