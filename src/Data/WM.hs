{-# LANGUAGE PatternGuards #-}

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
    , wmEmpty
    ) where

import Data.WM.Window
import Data.WM.Workspace
import Data.WM.Screen

data WM t l w s ctx = WM
    { wmCurrent :: !(Screen t l w s ctx)
    , wmVisible :: [Screen t l w s ctx]
    , wmHidden  :: [Workspace t l s]
    } deriving (Show, Read, Eq)

wmEmpty :: (Integral s) => l -> [t] -> [ctx] -> WM t l w s ctx
wmEmpty layout tags descs
    | null tags                  = abort "no tags given"
    | length descs > length tags = abort "more screens than tags"
    | null descs                 = abort "no screens given"
    | otherwise                  = WM cur rest hidden
    where
        (seen, hidden) = splitAt (length descs) $ map (\t -> Workspace t layout Nothing) tags
        (cur:rest)     = [ Screen wksp id desc | (wksp, id, desc) <- zip3 seen [0 .. ] descs ]

-- Helper functions

abort :: String -> a
abort = error . ("WM: " ++)
