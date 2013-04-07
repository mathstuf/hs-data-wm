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
    , wsFlatten
    , Workspace(..)
    , wkFlatten
    ) where

import Data.WM.Window

class WindowStack s where
    wsFocused  :: s w -> w
    wsPrevious :: s w -> [w]
    wsNext     :: s w -> [w]

wsFlatten :: (WindowStack s) => s w -> [w]
wsFlatten =
    withStack $ \l c r -> reverse l ++ c : r

data Workspace t l s = Workspace
    { wkTag    :: !t
    , wkLayout :: l
    , wkStack  :: Maybe s
    } deriving (Show, Read, Eq)

wkFlatten :: (WindowStack s) => Workspace t l (s w) -> [w]
wkFlatten =
    maybe [] wsFlatten . wkStack

-- Helper functions

withStack :: (WindowStack s) => ([w] -> w -> [w] -> a) -> s w -> a
withStack f stack =
    f l c r
    where
        l = wsPrevious stack
        c = wsFocused stack
        r = wsNext stack
