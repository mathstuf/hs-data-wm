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
    , wmViewWorkspace
    , wmFocusWorkspace
    , wmWithCurrentStack
    , wmModifyCurrentStack
    , wmMaybeModifyCurrentStack
    , wmFocusedWindow
    , wmFocusedTag
    , wmFocusedScreen
    ) where

import Data.List
import Data.Maybe

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

-- Operations

wmViewWorkspace :: (Eq t, Eq s) => t -> WM t l w s ctx -> WM t l w s ctx
wmViewWorkspace tag wm
    -- Current tag?
    | tag == wmFocusedTag wm =
        wm
    -- Visible on a screen?
    | Just scr <- find ((tag ==) . screenTag) visible =
        wm { wmCurrent = scr
           , wmVisible = current : deleteBy (eq scrId) scr visible
           }
    -- Hidden?
    | Just wksp <- find ((tag ==) . wkTag) hidden =
        wm { wmCurrent = current { scrWorkspace = wksp }
           , wmHidden = (scrWorkspace current) : deleteBy (eq wkTag) wksp hidden
           }
    -- Didn't find the tag.
    | otherwise =
        wm
    where
        eq f    = \x y -> f x == f y
        current = wmCurrent wm
        visible = wmVisible wm
        hidden  = wmHidden wm

wmFocusWorkspace :: (Eq t, Eq s) => t -> WM t l w s ctx -> WM t l w s ctx
wmFocusWorkspace tag wm
    -- Hidden already?
    | any wkspTag hidden =
        wmViewWorkspace tag wm
    -- Focus the screen with the tag.
    | Just scr <- find ((tag ==) . screenTag) visible =
        wm { wmCurrent = current { scrWorkspace = scrWorkspace scr }
           , wmVisible = scr : filter (not . wkspTag . scrWorkspace) visible
           }
    -- Didn't find the tag.
    | otherwise =
        wm
    where
        wkspTag = (tag ==) . wkTag
        current = wmCurrent wm
        visible = wmVisible wm
        hidden  = wmHidden wm

-- Higher order functions

wmWithCurrentStack :: a -> (s -> a) -> WM t l w s ctx -> a
wmWithCurrentStack def f =
    maybe def f . wkStack . scrWorkspace . wmCurrent

wmModifyCurrentStack :: Maybe s -> (s -> Maybe s) -> WM t l w s ctx -> WM t l w s ctx
wmModifyCurrentStack def f wm =
    wm { wmCurrent = current { scrWorkspace = wksp { wkStack = wmWithStack def f wm } } }
    where
        current = wmCurrent wm
        wksp    = scrWorkspace current

wmMaybeModifyCurrentStack :: (s -> s) -> WM t l w s ctx -> WM t l w s ctx
wmMaybeModifyCurrentStack f =
    wmModifyStack Nothing (Just . f)

-- Query functions

wmFocusedWindow :: (WindowStack s) => WM t l w (s w) ctx -> Maybe w
wmFocusedWindow =
    wmWithStack Nothing (Just . wsFocused)

wmFocusedTag :: WM t l w s ctx -> t
wmFocusedTag = screenTag . wmCurrent

wmFocusedScreen :: WM t l w s ctx -> s
wmFocusedScreen = scrId . wmCurrent

wmScreenTag :: (Eq s) => s -> WM t l w s ctx -> Maybe t
wmScreenTag sid wm =
    listToMaybe [ wkTag wk | Screen wk s _ <- wmCurrent wm : wmVisible wm
                           , s == sid ]

-- Helper functions

screenTag :: Screen t l w s ctx -> t
screenTag = wkTag . scrWorkspace

abort :: String -> a
abort = error . ("WM: " ++)
