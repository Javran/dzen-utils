-- |
-- Module      :  System.Dzen.Internal
-- Copyright   :  (c) 2009 Felipe A. Lessa
-- License     :  GPL 3 (see the LICENSE file in the distribution)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable (MPTC and type families)
--
-- Internal data types and functions that are not exported
-- to the outside world.

module System.Dzen.Internal
    (-- * State
     DSt(..)
    ,DColour

     -- * DString
    ,DString(..)
    ,rawStr
    ,toString
    ,size
    ,mkCmd

     -- * Printer
    ,Printer(..)
    ,apply
    ,Transform(..)
    ) where


import Control.Arrow
import Control.Monad
import Data.Colour
import Data.String
import Data.Monoid


-- | The internal state we maintain. Currently it only contains
--   the foreground and the background colours and if we are
--   ignoring the background or not.
--
--   This state is passed around like a @Reader@ monad, each
--   function receives it and does whatever it want, and not
--   like a @State@ monad!
data DSt = S {sFg :: !(Maybe DColour)
             ,sBg :: !(Maybe DColour)
             ,sIgnoreBg :: !Bool}

-- | Our colours.
type DColour = Colour Double

-- | Empty state.
emptyState :: DSt
emptyState = S Nothing Nothing True





-- | A @DString@ is used for constant string output, see 'str'.
--   The @D@ on @DString@ stands for @dzen@, as these strings
--   may change depending on the state (and that's why you
--   shouldn't rely on 'Show', as it just uses an empty state)
newtype DString = DS {unDS :: DSt -> (String -> String, Maybe Int)}
-- A differencial list of chars (i.e. ShowS) and the number of chars.
--
-- Note that we use the @DStrings@ by themselves (i.e. concatenating
-- with @Printers@) and for output of the @Printers@, but state is
-- relevant only on the former. The @DString@s returned by Printers
-- always get passed the @emptyState@. Of course it would be better to
-- create two distinct data types, but we'll stick to this semantic
-- hole for now.

instance IsString DString where
    fromString = DS . const . escape 0

instance Show DString where
    show (DS ds) = concat ["<with empty state: ",
                           show ((fst $ ds emptyState) ""), ">"]

instance Monoid DString where
    mempty = DS $ const (id, Just 0)
    mappend (DS ds1) (DS ds2) = DS $ \st -> ds1 st # ds2 st
                        -- Note how we duplicate 'st' above
        where (s1,n1) # (s2,n2) = (s1 . s2, liftM2 (+) n1 n2)

escape :: Int -> String -> (String -> String, Maybe Int)
escape n s | n `seq` s `seq` False = error "escape: never here"
escape n ('^':xs) = first (t.t) $ escape (n+1) xs where t = (.) ('^':)
escape n ( x :xs) = first x'    $ escape (n+1) xs where x' = (.) (x:)
escape n []       = (id, Just n)


-- | Converts a @String@ into a @DString@ without escaping anything.
--   You /really/ don't need to use this, trust me!
rawStr :: String -> DString
rawStr str = DS $ const ((str ++), Just $ length str)

-- | Converts a @DString@ back into a @String@. Note that
--   @(toString . rawStr)@ is not @id@, otherwise @toString@
--   would not work in some cases.
--   Probably you don't need to use this, unless you want
--   something like a static bar and nothing else.
toString :: DString -> String
toString = ("^ib(1)" ++) . ($ "") . fst . ($ emptyState) . unDS

-- | Tries to get the number of characters of the @DString@.
--   May return @Nothing@ when there are graphical objects.
--   Probably you don't need to use this function.
size :: DString -> Maybe Int
size = snd . ($ emptyState) . unDS
--   We apply a new empty state but that shouldn't be a problem
--   because currently all functions that depend on the state
--   do not change the size.

-- | @mkCmd graph cmd arg@ creates a command string like
--   @\"^cmd(arg)\"@. If @graph@ is @False@ then we give length zero
--   to the resulting @DString@, otherwise we don't give a length
--   (which propagates for strings concatenated to this). You should
--   use @False@ whenever possible.
mkCmd :: Bool -> String -> String -> DString
mkCmd graph cmd arg = DS $ const (str, len)
    where str = ('^':).(cmd++).('(':).(arg++).(')':)
          len = if graph then Nothing else Just 0


-- | A printer is used when the output depends on an input, so a
--   @Printer a@ generates a 'DString' based on some input of
--   type @a@ (and possibly updates some internal state).
newtype Printer a = P {unP :: DSt -> a -> (DString, Printer a)}
-- We don't use a Reader just because we already have
-- to do a lot of pumbling ourselves anyway.

-- | Apply a printer to an appropriate input, returning
--   the output string and the new printer.
apply :: Printer a -> a -> (String, Printer a)
apply p i = first toString . ($ i) . ($ emptyState) . unP $ p
-- We have apply here in Internal because it uses 'emptyState',
-- which we don't want to export because its defaults are not
-- the same as dzen's defaults (we use "ib(1)" by default).


-- | @Transform@ is a specialization of @Functor@ for @DString@s.
--   This class is used for functions that may receive @DString@
--   or @Printer a@ as an argument because they operate only
--   on their outputs and internal states (and not on the inputs).
--
--   So, whenever you see a function of type
--
--   > func :: Transform a => Blah -> Bleh -> a -> a
--
--   it means that @func@ can be used in two ways:
--
--   > func :: Blah -> Bleh -> DString -> DString
--   > func :: Blah -> Bleh -> Printer a -> Printer a  -- Printer of any input!
--
--   Try to have this in mind when reading the types.
--
--   Note: There is also a non-exported @transformSt@ for
--   transforming the state in this class, otherwise it would
--   be meaningless to use a class only for @transform@ (it
--   would be better to make @liftT :: (DString -> DString)
--   -> (Printer a -> Printer a)@).
class Transform a where
    -- | This function is 'id' on @DString@ and
    --   modifies the output of a @Printer a@.
    transform :: (DString -> DString) -> (a -> a)
    transform f = transformSt (\st -> (st, f))

    transformSt :: (DSt -> (DSt, DString -> DString)) -> (a -> a)

instance Transform DString where
    transform = id
    transformSt f ds = DS $ \st -> let (st', dsT) = f st
                                   in unDS (dsT ds) st'

instance Transform (Printer a) where
    transform f = P . (((f *** transform f) .) .) . unP
    transformSt f (P p) = P $ \st i -> let (st', dsT) = f st
                                           (ds, p') = p st' i
                                       in (dsT ds, transformSt f p')

