-----------------------------------------------------------------------------
-- |
-- Module      :  Data.WM.TilingStack
-- Copyright   :  (c) Ben Boeckel 2013
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  mathstuf@gmail.com
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--

module Data.WM.TilingStack
    ( TilingStack
    , tsSwapFocus
    ) where

import Data.WM.FloatStack
import Data.WM.Workspace

data TilingStack w = TilingStack
    { tsFocus      :: !w
    , tsPrevious   :: [w]
    , tsNext       :: [w]
    , tsFloating   :: Maybe (FloatStack w)
    , tsFloatFocus :: !Bool
    } deriving (Show, Read, Eq)

instance WindowStack TilingStack where
    wsFocused  ts =
        case tsFloatFocus ts of
            True  -> tsFocus ts
            False -> case tsFloating ts of
                        Just fs -> wsFocused fs
                        Nothing -> abort "failed to track floaing focus"
    wsPrevious ts =
        case tsFloatFocus ts of
            True  -> tsPrevious ts
            False -> case tsFloating ts of
                        Just fs -> wsPrevious fs
                        Nothing -> abort "failed to track floaing focus"
    wsNext     ts =
        case tsFloatFocus ts of
            True  -> tsNext ts
            False -> case tsFloating ts of
                        Just fs -> wsNext fs
                        Nothing -> abort "failed to track floaing focus"

tsSwapFocus :: TilingStack w -> TilingStack w
tsSwapFocus ts =
    case tsFloatFocus ts of
        True  -> ts { tsFloatFocus = False }
        False -> case tsFloating ts of
                    Just fs -> ts { tsFloatFocus = True }
                    Nothing -> ts

abort :: String -> a
abort = error . ("TilingStack: " ++)
