-- |
-- Module      :  System.Dzen.Base
-- Copyright   :  (c) 2009 Felipe A. Lessa
-- License     :  GPL 3 (see the LICENSE file in the distribution)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable (MPTC and type families)
--
-- This module contains most of the basic functions of
-- this package. The data types presented here are:
--
--  ['DString'] strings that support constant time concatenation,
--    dzen attributes and some instropection.
--
--  ['Printer'] encapsulates functions take take some input and
--    produce a @DString@ as a result, allowing them to be
--    combined and applied.

module System.Dzen.Base
    (-- * Dzen Strings
     DString
    ,str
    ,rawStr
    ,toString
    ,size
    ,parens

     -- * Printers
    ,Printer
    ,comap
    ,simple
    ,simple'
    ,inputPrinter
    ,inputPrinter'
    ,cstr
    ,cshow

     -- * Combining printers
    ,Combine(..)
     -- $combine
    ,(+=+)
    ,(+-+)
    ,(+/+)
    ,(+<+)
    ,combine

     -- * Applying printers
     -- $apply
    ,apply
    ,applyMany
    ,applyMany_
    ,applyForever

     -- * Transforming
    ,Transform(transform)
    ) where

import Prelude hiding ((++))
import Control.Arrow hiding ((+++))
import Data.Function
import Data.String
import Data.Monoid

import System.Dzen.Internal





-- | Converts a @String@ into a @DString@, escaping characters if
--   needed. This function is used in 'fromString' from 'IsString',
--   so @DString@s created by @OverloadedStrings@ extension will
--   be escaped.
str :: String -> DString
str = fromString

-- | Used internally, use 'mappend'.
(++) :: Monoid a => a -> a -> a
(++) = mappend

-- | @parens open close d@ is equivalent to @mconcat [open, d, close]@.
parens :: DString -> DString -> DString -> DString
parens open close d = open ++ d ++ close







-- | A @Printer@ is a cofunctor.
comap :: (a -> b) -> (Printer b -> Printer a)
comap f (P dp) = P $ \st input -> let (out,dp') = dp st (f input)
                                  in (out, comap f dp')

-- | Constructs a @Printer@ that depends only on the input.
simple :: (a -> DString) -> Printer a
simple f = fix $ P . const . (. f) . flip (,)

-- | Like 'simple', but using @String@s.
simple' :: (a -> String) -> Printer a
simple' = simple . (str .)

-- | Constructs a @Printer@ that depends on the current
--   and on the previous inputs.
inputPrinter :: (b -> a -> (DString, b)) -> b -> Printer a
inputPrinter f b = P . const $ second (inputPrinter f) . f b

-- | Like 'inputPrinter', but with @String@s.
inputPrinter' :: (b -> a -> (String, b)) -> b -> Printer a
inputPrinter' = inputPrinter . ((first str .) .)

-- | Works like 'str', but uses the input instead of being
--   constant. In fact, it is defined as @simple str@.
cstr :: Printer String
cstr = simple str

-- | Same as @simple' show@.
cshow :: Show a => Printer a
cshow = simple' show









-- | Class used for combining @DString@s and @Printer@s
--   exactly like 'mappend'.
--
--   Note that we don't lift @DString@ to @Printer ()@ and use a
--   plain function of type @Printer a -> Printer b
--   -> Printer (a,b)@ because that would create types such as
--   @Printer ((),(a,((),(b,()))))@ instead of
--   @Printer (a,b)@.
class Combine a b where
    -- | The type of the combined input of @a@ with @b@.
    type Combined a b :: *

    -- | Combine @a@ into @b@. Their outputs are concatenated.
    (+++) :: a -> b -> Combined a b

infixr 4 +++
infixr 4 +=+
infixr 4 +-+
infixr 4 +/+
infixr 4 +<+

instance Combine DString DString where
    type Combined DString DString = DString
    (+++) = (++)

instance Combine DString (Printer a) where
    type Combined DString (Printer a) = Printer a
    ds1 +++ (P dp2) =
        P $ \st input -> let (out2,dp2') = dp2 st input
                         in (ds1 ++ out2, ds1 +++ dp2')

instance Combine (Printer a) DString where
    type Combined (Printer a) DString = Printer a
    (P dp1) +++ ds2 =
        P $ \st input -> let (out1,dp1') = dp1 st input
                         in (out1 ++ ds2, dp1' +++ ds2)

instance Combine (Printer a) (Printer b) where
    type Combined (Printer a) (Printer b) = Printer (a,b)
    (+++) = combine id

-- $combine
--
-- We currently have the following @Combined@ types:
--
-- > type Combined DString    Dstring      = DString
-- > type Combined DString    (Printer a)  = Printer a
-- > type Combined (Printer a) DString     = Printer a
-- > type Combined (Printer a) (Printer b) = Printer (a,b)
--
-- For example, if @a :: DString@, @b,e :: Printer Int@,
-- @c :: Printer Double@ and @d :: DString@, then
--
-- > (a +++ b +++ c +++ d +++ e) :: Printer (Int, (Double, Int))


-- | Sometimes you want two printers having the same input,
--   but @p1 +++ p2 :: Printer (a,a)@ is not convenient. So
--   @p1 +=+ p2 :: Printer a@ works like '+++' but gives
--   the same input for both printers.
(+=+) :: Printer a -> Printer a -> Printer a
(+=+) = combine (\x -> (x, x))

-- | Works like '+=+' but the second printer's input is a tuple.
(+-+) :: Printer a -> Printer (a,b) -> Printer (a,b)
(+-+) = combine (\x -> (fst x, x))


-- | While you may say @p1 +=+ (ds1 +++ ds2 +++ p2)@,
--   where @p1,p2 :: Printer a@ and @ds1,ds2 :: DString@,
--   you can't say @p1 +=+ (po +++ p2)@ nor
--   @(p1 +++ po) +=+ p2@ where @po :: Printer b@.
--
--   This operator works like '+++' but shifts the
--   tuple, giving you @Printer (b,a)@ instead of
--   @Printer (a,b)@. In the example above you may
--   write @p1 +>+ po +/+ p2@.
(+/+) :: Printer a -> Printer b -> Printer (b,a)
(+/+) = combine (\(a,b) -> (b,a))


-- | This operator works like '+/+' but the second
--   printer's input is a tuple. Use it like
--
--   > pA1 +-+ pB +<+ pC +<+ pD +/+ pA2 :: Printer (a,(b,(c,d)))
--
--   where both @pA1@ and @pA2@ are of type @Printer a@.
(+<+) :: Printer a -> Printer (b,c) -> Printer (b,(a,c))
(+<+) = combine (\(b,(a,c)) -> (a,(b,c)))


-- | This is a general combine function for @Printer@s.
--   The outputs are always concatenated, but the inputs
--   are given by the supplied function.
--
--   The combining operators above are defined as:
--
--   > (+++) = combine id    -- restricted to Printers
--   > (+=+) = combine (\x -> (    x, x))
--   > (+-+) = combine (\x -> (fst x, x))
--   > (+/+) = combine (\(a,b)     -> (b,a))
--   > (+<+) = combine (\(b,(a,c)) -> (a,(b,c)))
--
--   Note also the resamblence with 'comap'. In fact,
--   if we have @(+++)@ and @comap@ we may define
--
--   > combine f a b = comap f (a +++ b)       -- pointwise
--   > combine = flip (.) (+++) . (.) . comap  -- pointfree
--
--   and with @combine@ and @simple@ we may define
--
--   > comap f = combine (\i -> ((), f i)) (simple $ const mempty) -- pointwise
--   > comap = flip combine (simple $ const mempty) . ((,) () .)   -- pointfree
combine :: (c -> (a, b)) -> Printer a -> Printer b -> Printer c
combine split = f
  where f (P dp1) (P dp2) =
            P $ \st input -> let (input1, input2) = split input
                                 (out1, dp1') = dp1 st input1
                                 (out2, dp2') = dp2 st input2
                             in (out1 ++ out2, f dp1' dp2')
                     -- Again, note how state is duplicated
{-# INLINE combine #-}




-- $apply
--
-- Note that applying should be the /last thing/ you do,
-- and you should /never/ apply inside a 'DString'
-- or 'Printer'. Doing so may cause undefined behaviour
-- because both @DString@ and @Printer@ contain some internal
-- state. We create a fresh internal state when applying,
-- so applying inside them will not take their internal
-- state into account. You've been warned!


-- | Apply a printer many times in sequence. Most of the
--   time you would ignore the final printer using
--   'applyMany_', but it can be used to continue applying.
applyMany :: Printer a -> [a] -> ([String], Printer a)
applyMany p (i:is) = let (s,p') = apply p i
                         rest = applyMany p' is
                     in (s : fst rest, snd rest)
applyMany p [] = ([], p)


-- | Like 'applyMany' but ignoring the final printer.
applyMany_ :: Printer a -> [a] -> [String]
applyMany_ p (i:is) = let (s,p') = apply p i in s : applyMany_ p' is
applyMany_ _ [] = []


-- | Apply a printer forever inside a monad. The first action
--   is used as a supply of inputs while the second action
--   receives the output before the next input is requested.
--
--   Note that your supply may be anything. For example,
--   inside @IO@ you may use @threadDelay@:
--
--   > applyForever (threadDelay 100000 >> getInfo) (hPutStrLn dzenHandle)
applyForever :: Monad m => Printer a -> m a -> (String -> m ()) -> m ()
applyForever p get act = get >>= uncurry (>>) . (act *** f) . apply p
    where f p' = applyForever p' get act
