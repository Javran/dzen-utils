-- |
-- Module      :  System.Dzen.Padding
-- Copyright   :  (c) 2009 Felipe A. Lessa
-- License     :  GPL 3 (see the LICENSE file in the distribution)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable (uses MPTC and type families)
--
-- This is a handy module with functions for manual and automatic
-- padding. To pad means to force the length of a string to be of a
-- minimum size by adding /padding characters/ on either or both sides
-- of the string (usually spaces). For example, padding the string
-- @\"123\"@ to have length of 10 characters would give us the string
--
-- > "       123"   -- if padding on the left
-- > "123       "   -- if padding on the right
-- > "    123   "   -- if padding on both sides
--
-- We provide two kinds of padding here:
--
-- [manual padding] This is the kind of padding you usually see
-- in other (non-@dzen@) libraries. You give them the type of
-- padding and the minimum length that you want and they will
-- give you back another string. The @pad*@ functions do this
-- with both plain @DString@s and with the output of @Printer@s.
--
-- [automatic padding] This is the same as a \"never shrink\"
-- padding. An automatic padder adjusts its minimum length
-- to be at least the greatest length it has seen so far, which
-- means that an auto-padded @Printer@ will never shrink its
-- size. This is very useful if you don't want everything swinging
-- on your bar everytime the bar is updated.
module System.Dzen.Padding
    (-- * Manual padding
     -- $padWarning
     padL
    ,padR
    ,padC
    ,pad
    ,PadWhere(..)

     -- * Automatic padding
     -- $autoPad
    ,autoPadL
    ,autoPadR
    ,autoPadC
    ,autoPad
    ) where

import System.Dzen.Internal
import System.Dzen.Base

-- $padWarning
--
-- Note that there are commands that generate graphical
-- outputs, such as 'System.Dzen.Graphics.rect', and we can't
-- tell how many \"characters\" a graphic object has.
-- Whenever you apply any of the padding functions below
-- to a @DString@ that contains one of these graphical
-- objects, there will be no padding. Note that colours
-- do not affect padding as they do not have any width
-- (and we don't mistake the command characters with
-- the characters that will be shown).

-- | Pads the given @DString@ or @Printer@ output
--   with spaces to be at least @n@ chars in length
padL :: Transform a => Int -> (a -> a)
padL = pad ' ' PadLeft

-- | Same as 'padL', but insert spaces on the right of the string.
padR :: Transform a => Int -> (a -> a)
padR = pad ' ' PadRight

-- | Same as 'padL', but insert spaces on both sides,
--   trying to keep the original contents in the middle.
padC :: Transform a => Int -> (a -> a)
padC = pad ' ' PadCenter

-- | Generic pad function, padding with any character
--   and in any place.
pad :: Transform a => Char -> PadWhere -> Int -> (a -> a)
pad c w n = transform $ DS . (pad' .) . unDS
    where
      pad' (string, Just k) | k < n =
          (case w of
             PadCenter -> repli (d+m) . string . repli d
             PadLeft   -> repli a . string
             PadRight  -> string . repli a, Just n)
          where a = n-k; (d,m) = a `divMod` 2
                repli = foldr (.) id . flip replicate (c:)
      pad' other = other

-- | Where to add the padding characters.
data PadWhere = PadLeft | PadRight | PadCenter





-- $autoPad
--
-- Automatic padding adjusts the number of padding characters
-- dinamically, increasing the pad everytime the string
-- size is greater than the pad size. For example, if
-- you give @autoPadL 3@ the following strings
--
-- > "1"
-- > "12"
-- > "123"
-- > "1234"
-- > "12345"
-- > "1234"
-- > "12"
-- > ""
--
-- then it will give the following outputs
--
-- > "  1"
-- > " 12"
-- > "123"
-- > "1234"
-- > "12345"
-- > " 1234"
-- > "   12"
-- > "     "
--
-- Using @autoPadC 3@ would give
--
-- > " 1 "
-- > " 12"
-- > "123"
-- > "1234"
-- > "12345"
-- > " 1234"
-- > "  12 "
-- > "     "
--
-- Some notes:
--
-- - If you're lazy you may give an initial number
--   of zero and after some inputs the padding will be fine.
--
-- - If the automatic pad finds out that there is a graphical
--   object at some string, then it will continue trying to
--   pad the next strings. Although normally the strings will
--   all contain graphics or not, we consider that the performance
--   loss is negligible (and we do what the user expects).

-- | Automatic padding for 'padL'.
autoPadL :: Int -> (Printer a -> Printer a)
autoPadL = autoPad ' ' PadLeft

-- | Automatic padding for 'padR'.
autoPadR :: Int -> (Printer a -> Printer a)
autoPadR = autoPad ' ' PadRight

-- | Automatic padding for 'padC'.
autoPadC :: Int -> (Printer a -> Printer a)
autoPadC = autoPad ' ' PadCenter

-- | Generic automatic padding function, analog to 'pad'.
autoPad :: Char -> PadWhere -> Int -> (Printer a -> Printer a)
autoPad c w n pr =
    P $ \st input -> let (output, pr') = unP pr st input
                         s = maybe 0 id $ size output
                     in case s `compare` n of
                          LT -> (pad c w n output, autoPad c w n pr')
                          _  -> (output,           autoPad c w s pr')
