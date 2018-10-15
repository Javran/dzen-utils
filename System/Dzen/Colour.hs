-- |
-- Module      :  System.Dzen.Colour
-- Copyright   :  (c) 2009 Felipe A. Lessa
-- License     :  GPL 3 (see the LICENSE file in the distribution)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable (uses MPTC and type families)
--
-- Support for colours. This module is entirely based on the @colour@
-- package, so we strongly recommend that you at least
--
-- > import qualified Data.Colour.Names as C
--
-- which will import various aliases for creating 'Colour's.
--
-- Note changing the colours using the functions below do not hinder
-- the use of automatic padding.

module System.Dzen.Colour
    (-- * Changing colours
     DColour
    ,fg
    ,bg

     -- * Reseting to the defaults
    ,defFg
    ,defBg

     -- * Change or reset
    ,changeFg
    ,changeBg
    ) where

import Data.Colour.SRGB
import System.Dzen.Base
import System.Dzen.Internal




-- | Set the foreground colour. Note that the foreground
--   colour is changed only inside the transformed @DString@
--   or @Printer@, unlike using @\"^fg\"@ which may affect
--   subsequent strings.
--
--   So you may write
--   @fg 'black' (fg 'lime' (str \"lime\") +++ str \"black\")@
--   and it works like you expect it to.
fg :: Transform a => DColour -> (a -> a)
fg = changeFg . Just

-- | Like 'fg', but set the background colour.
bg :: Transform a => DColour -> (a -> a)
bg = changeBg . Just




-- | Set the foreground colour to be the default one,
--   which is specified as a parameter to dzen (outside
--   the control of the printers).
defFg :: Transform a => a -> a
defFg = changeFg Nothing

-- | Like 'defFg', but for the background colour.
defBg :: Transform a => a -> a
defBg = changeBg Nothing




-- | Set the foreground to be a specified one (@Just c@) or
--   the dzen's default (@Nothing@). Both 'fg' and 'defFg'
--   are specializations of this function.
changeFg :: Transform a => Maybe DColour -> (a -> a)
changeFg c = transformSt $ \st ->
             if sFg st == c then (st, id)
             else (st {sFg = c}, parensF showFg c (sFg st))

-- | Like 'changeFg', but for the background colour.
changeBg :: Transform a => Maybe DColour -> (a -> a)
changeBg c = transformSt $ \st ->
             if sBg st == c then (st, id)
             else (st {sBg = c}, parensF showBg c (sBg st))






-- Internal functions

-- | 'parens' with steroids.
parensF :: (a -> DString) -> a -> a -> DString -> DString
parensF f = \a b -> parens (f a) (f b)

-- | Change the foreground colour.
showFg :: Maybe DColour -> DString
showFg = mkCmd False "fg" . fromColour

-- | Change the background colour.
showBg :: Maybe DColour -> DString
showBg = mkCmd False "bg" . fromColour

-- | Lift 'sRGB24shows'.
fromColour :: (RealFrac a, Floating a) => Maybe (Colour a) -> String
fromColour = maybe "" sRGB24show