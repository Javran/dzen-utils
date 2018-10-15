-- |
-- Module      :  System.Dzen
-- Copyright   :  (c) 2009 Felipe A. Lessa
-- License     :  GPL 3 (see the LICENSE file in the distribution)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable (uses MPTC and type families)
--
-- Hello!
--
-- This is @dzen-utils@' main module. It re-exports every other
-- module from this library, so you may just say
--
-- > import System.Dzen
--
-- and you'll have everything in hand. To learn more about this
-- library, please see the documentation of each module exported here.
-- To get you started, there are some simple examples below. :)

module System.Dzen
    (-- * Example 1
     -- $example1p1

     -- ** Constructing - Part 1
     -- $example1p2

     -- ** Constructing - Part 2
     -- $example1p3

     -- ** Applying
     -- $example1p4

     -- ** Whole code
     -- $example1p5



     -- * Example 2
     -- $example2p1

     -- ** Separator
     -- $example2p2

     -- ** Time bar
     -- $example2p3

     -- ** Glueing the time text with the bar
     -- $example2p4

     -- ** Whole code
     -- $example2p5


     -- * Module exports
     module System.Dzen.Base
    ,module System.Dzen.Colour
    ,module System.Dzen.Graphics
    ,module System.Dzen.Padding
    ,module System.Dzen.Bars
    ,module System.Dzen.Process
    ) where

import System.Dzen.Base
import System.Dzen.Colour
import System.Dzen.Graphics
import System.Dzen.Padding
import System.Dzen.Bars
import System.Dzen.Process


-- $example1p1
--
-- Suppose you have
--
-- > import Data.Time
-- > import System.Locale
-- >
-- > getZonedTime     :: IO ZonedTime -- from time package
-- > getCurrentWindow :: IO String    -- from somewhere you know :)
--
-- and that you want your bar to look like
--
-- > "Wndw: [my window title here] - Sun 15 Mar 07:10:02"
--
-- Okay, that's pretty nice. What you will do first will be to
-- construct the 'Printer' of your bar. Basically, what you want is to
-- concatenate @\"Wndw: \"@, the result of @getCurrentWindow@, the
-- separator, and the formated result of @getZonedTime@. The @Printer@
-- we'll build below contain all the information necessary to create
-- the final output string above from the two @get@ functions!


-- $example1p2
--
-- To write the constant strings, we use 'str':
--
-- > str          :: String -> DString
-- > str "Wndw: " :: DString
-- > str " - "    :: DString
--
-- That is pretty straightforward. But how can we print the current
-- window? We will just use 'cstr' here
--
-- > cstr :: Printer String
--
-- While @str@ is used as a common function, you just apply the
-- string you want to it, @cstr@ will get the string from another
-- source. What we'll do shortly is to pipe @IO String@ into
-- @Printer String@ to obtain our final output. Instead of
-- @Printer a@, think of it as @Printer input@.
--
-- To concatenate we just have to use '+++' from 'Combine', which
-- is an ugly class used to create beautiful types:
--
-- > str "Wndn: " +++ cstr +++ str " - "  :: Printer String
--
-- Whenever you concatenate a @DString@ with a @Printer input@, you
-- get a @Printer input@ as a result. That is, if you concatenate a
-- constant string to something that takes an input and produces a
-- string, what you get is something that takes an input and produces
-- the concatenated string. Note that you don't need to write the
-- type above.

-- $example1p3
--
-- Now we can't just go ahead and use @cstr@ directly with
-- @getZonedTime@ because, unlike @getCurrentWindow@, its result is
-- not a string, but a @ZonedTime@. To format the @ZonedTime@ is
-- outside the scope of this example, but you can get the format
-- shown above with
--
-- > format :: ZonedTime -> String
-- > format = formatTime defaultTimeLocale "%a %e %b %H:%M:%S"
--
-- While you can write
--
-- > fmap format getZoneTime :: IO String
--
-- you don't need to push the format function into the @IO@ monad,
-- just put it in the @Printer@, the place where it belongs! To
-- accomplish that use 'simple'' instead of 'cstr':
--
-- > simple'        :: (input -> String) -> Printer input
-- > simple' format :: Printer ZonedTime
--
-- And it's done! Concatenating everything:
--
-- > printer :: Printer (String, ZonedTime)
-- > printer = str "Wndn: " +++ cstr +++ str " - " +++ simple' format
--
-- Whenever you concatenate @Printer a@ with @Printer b@ you
-- get @Printer (a,b)@.

-- $example1p4
--
-- In the end we just want strings, not @Printer@s, so we need to
-- apply our @printer@ to some inputs. We already have functions for
-- each part of our input, but we need to combine them. We may just
-- use @##@ from "System.Dzen.Process" in this case (and in most others as
-- well):
--
-- > supply :: IO (String, ZonedTime)
-- > supply = getCurrentWindow ## getZonedTime
--
-- Now we could use 'apply', 'applyMany' or 'applyForever' to
-- get strings out of our printer, but we'll use 'runDzen' directly
-- which is a tad easier:
--
-- > myDzen :: IO ()
-- > myDzen = runDzen "dzen2" [] 500 printer supply
--
-- And that's it! You may use @myDzen@ directly as @main@, or you
-- may use @forkIO myDzen@ inside @xmonad@. Pretty nice, uh?

-- $example1p5
--
-- This is the whole code plus a stub @getCurrentWindow@ and a @main@.
-- We omit the signatures on purpose to show how you could have
-- written it lazily. You may also get the file @Example1.hs@ from the
-- source code which contains all the type signatures.
--
-- >import Data.Time
-- >import System.Dzen
-- >import System.Locale
-- >
-- >printer = str "Wndn: " +++ cstr +++ str " - " +++ simple' format
-- >format  = formatTime defaultTimeLocale "%a %e %b %H:%M:%S"
-- >supply  = getCurrentWindow ## getZonedTime
-- >myDzen  = runDzen "dzen2" [] 500 printer supply
-- >
-- >getCurrentWindow = return "My =^.^= Lolcats"
-- >main = myDzen





-- $example2p1
--
-- Now we want to do something different: let's have a graphical
-- bar for our clock! And, why not, a graphical separator.

-- $example2p2
--
-- The graphical separator will be very simple. Instead of dash
-- (@\" - \"@) we'll use a small dot:
--
-- > import Data.Monoid
-- >
-- > sep :: DString
-- > sep = mconcat [pos 4, rect 3 3, pos 4]
--
-- This is a 3x3 rectangle with 4 pixels of spacing on each side.
-- And, thats it!

-- $example2p3
--
-- Now something more challenging. Instead of showing the minutes
-- and the seconds, we want to show a graphical bar, something like
--
-- > "Wndw: [my window title here] - Sun 15 Mar 07:[==    ]"
--
-- Well, that bar will show minutes and seconds, but we can
-- take as input just seconds, ranging from @0@ to @60*60-1@.
-- We'll use plain simple 'cgdbar', which mimics @gdbar@:
-- (please look at "System.Dzen.Bars" for more info)
--
-- > timeBar :: Printer Int
-- > timeBar = cgdbar False (40,10) Nothing Nothing True (0, 60*60-1)
--
-- But what we have is not the number of seconds, but a @ZonedTime@.
-- So we need
--
-- > zonedSecs :: ZonedTime -> Int
-- > zonedSecs = extract . localTimeOfDay . zonedTimeToLocalTime
-- >     where extract t = let minutes = fromIntegral (todMin t)
-- >                       in round (minutes * 60 + todSec t)
--
-- As @Printer@s are cofunctors, we can use 'comap' to get
-- a suitable @timeBar@:
--
-- > timeBar' :: Printer ZonedTime
-- > timeBar' = comap zonedSecs timeBar

-- $example2p4
--
-- We also want the rest of the time, not just the bar. First,
-- we need a new @format@, as we don't want to show the minutes
-- anymore:
--
-- > format :: ZonedTime -> String
-- > format = formatTime defaultTimeLocale "%a %e %b %H:"
--
-- You may be tempted to write @simple' format +++ timeBar'@ now,
-- but there's a small glitch: it would have type
-- @Printer (ZonedTime, ZonedTime)@, but we want both to use
-- the same @ZonedTime@. We could use 'comap' again or 'combine',
-- but it is easier to write
--
-- > time :: Printer ZonedTime
-- > time = simple' format +=+ timeBar'

-- $example2p5
--
-- The rest is just glue! So we now present the whole code,
-- again in a compact form. See @Example2.hs@ for the whole
-- code with type signatures:
--
-- >import Data.Monoid
-- >import Data.Time
-- >import System.Dzen
-- >import System.Locale
-- >
-- >zonedSecs = extract . localTimeOfDay . zonedTimeToLocalTime
-- >    where extract t = let minutes = fromIntegral (todMin t)
-- >                      in round (minutes * 60 + todSec t)
-- >
-- >time = simple' format +=+ comap zonedSecs timeBar
-- >  where format  = formatTime defaultTimeLocale "%a %e %b %H:"
-- >        timeBar = cgdbar False (40,10) Nothing Nothing True (0, 60*60-1)
-- >
-- >printer = str "Wndn: " +++ cstr +++ sep +++ time
-- >    where sep = mconcat [str " ", rectO 5 5, str " "]
-- >
-- >supply = getCurrentWindow ## getZonedTime
-- >myDzen = runDzen "dzen2" [] 500 printer supply
-- >
-- >getCurrentWindow = return "My =^.^= Lolcats"
-- >main = myDzen
