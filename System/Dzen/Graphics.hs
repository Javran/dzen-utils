-- |
-- Module      :  System.Dzen.Graphics
-- Copyright   :  (c) 2009 Felipe A. Lessa
-- License     :  GPL 3 (see the LICENSE file in the distribution)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable (uses MPTC and type families)
--
-- Support for the graphical abilities of @dzen@. Unfortunately
-- this is not the strongest are of @dzen@, so there isn't a lot
-- of functionality here, but there are \"bindings\" for every
-- function they provide.
--
-- You can draw:
--
-- [icons] With 'icon' (see @dzen@ documentation about formats and paths).
--
-- [rectanges] With 'rect' and 'rectO'.
--
-- [circles] With 'circ' and 'circO'.
--
-- Although it may seem that you can draw anything with the rectangle
-- primitive (i.e. in the worst case just use 1x1 rectangles to simulate
-- pixels), @dzen@ does not allow you to control the @y@ part of the
-- shapes, only the @x@ part (using 'pos' and 'absPos'): they are always
-- vertically centered.

module System.Dzen.Graphics
    (-- * Data types
     -- $dataTypes
     Width
    ,Height
    ,Radius

     -- * Icons
    ,icon

     -- * Shapes
    ,rect
    ,rectO
    ,circ
    ,circO

     -- * Positioning
    ,pos
    ,absPos

     -- * Misc
    ,ignoreBg
    )where

import Data.Monoid
import System.Dzen.Internal
import System.Dzen.Base


-- $dataTypes
--
-- These data types are used to hint the purpose of
-- each argument, making the type signatures more clear.

type Width  = Int
type Height = Int
type Radius = Int


-- | Draws an icon.
icon :: FilePath -> DString
icon = mkCmd True "i"


-- | @rect w h@ draws and fills a rectangle of width @w@
--   and height @h@. The rectangle is vertically centered
--   (that is, if @h == 1@ then it is a centered line,
--    something like @----@).
rect :: Width -> Height -> DString
rect = mkCmdX "r"

-- | Like @rect@, but only draws and does not fills (i.e.
--   draws an outline).
rectO :: Width -> Height -> DString
rectO = mkCmdX "ro"

-- | Internal.
mkCmdX :: String -> Width -> Height -> DString
mkCmdX cmd w h = mkCmd True cmd (show w ++ 'x':show h)


-- | @circ r@ draws and fils a circle of radius @r@, also
--   vertically centered.
circ :: Radius -> DString
circ = mkCmd True "c" . show

-- | Like @circ@, but does not fills.
circO :: Radius -> DString
circO = mkCmd True "co" . show


-- | @pos p@ moves the position of the next input @p@ pixels
--   to the right. Note that @p@ may be negative, effectively
--   moving to the right.
pos :: Int -> DString
pos 0 = mempty
pos n = mkCmd True "p" (show n)

-- | @absPos p@ moves the position of the next input to be
--   exactly @p@ pixels to the right of the initial position.
--   This should be used with care.
absPos :: Int -> DString
absPos = mkCmd True "pa" . show


-- | If @True@, the transformed @DString@ or @Printer@ will
--   ignore the background colour (i.e. it will draw over what
--   was already drawn). The default is @False@, the background
--   colour is used.
ignoreBg :: Transform a => Bool -> (a -> a)
ignoreBg ignore = transformSt $ \st ->
                  if sIgnoreBg st == ignore then (st, id)
                  else (st {sIgnoreBg = ignore},
                        parens (ib ignore) (ib $ sIgnoreBg st))
    where ib x = mkCmd False "ib" (if x then "1" else "0")

