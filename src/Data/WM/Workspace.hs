-----------------------------------------------------------------------------
-- |
-- Module      :  Data.WM.Workspace
-- Copyright   :  (c) Ben Boeckel 2013
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  mathstuf@gmail.com
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--

module Data.WM.Workspace
    ( WindowStack(..)
    , Workspace(..)
    ) where

import Data.WM.Window

class WindowStack s where
    wsFocused  :: s w -> w
    wsPrevious :: s w -> [w]
    wsNext     :: s w -> [w]

data Workspace t l s = Workspace
    { wkTag    :: !t
    , wkLayout :: l
    , wkStack  :: Maybe s
    } deriving (Show, Read, Eq)
