-- |
-- Module      :  System.Dzen.Process
-- Copyright   :  (c) 2009 Felipe A. Lessa
-- License     :  GPL 3 (see the LICENSE file in the distribution)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable (uses MPTC and type families)
--
-- Functions for creating supplies and running @dzen@.

module System.Dzen.Process
    (-- * Simple interface
     runDzen
    ,(##)

     -- * Powerful interface
    ,createDzen
    ,createDzen'
    ) where

import Control.Concurrent
import Control.Monad
import Data.Function
import System.IO
import System.Process hiding (proc)
import System.Dzen.Base


-- | Pipes a 'Printer' to a fresh instance of @dzen@. It runs
--   the following commands:
--
--   (1) Start @dzen@ with the supplied executable and arguments.
--
--   (2) Call the supply to get an input.
--
--   (3) Apply the input to the printer.
--
--   (4) Write the printer's output to @dzen@'s standard input.
--
--   (5) Sleeps for the specified delay using 'threadDelay'.
--
--   (6) Go back to step 2.
--
--   You may want to use this function inside a 'forkIO' if,
--   for example, you're inside @xmonad@.
runDzen :: FilePath  -- ^ Path to @dzen@ executable, probably @\"dzen2\"@
        -> [String]  -- ^ Arguments for @dzen@.
        -> Int       -- ^ Delay between suplies in milliseconds. May be zero.
        -> Printer a -- ^ @Printer@ to be used.
        -> IO a      -- ^ Supply of inputs.
        -> IO ()
runDzen path args delay printer get = do
  handle <- createDzen' path args
  let put s = hPutStrLn handle s >> threadDelay (delay * 1000)
  applyForever printer get put



-- | This is the same as @liftM2 (,)@, but with as a convenient
--   operator with right infixity (the same as '+++').  For example,
--   suppose you have printers
--
--   > prA :: Printer a
--   > prB :: Printer b
--   > prC :: Printer c
--
--   and supply functions
--
--   > getA :: m a
--   > getB :: m b
--   > getC :: m c
--
--   for some monad @m@. The final printer
--
--   > prFinal = prA +++ prB +++ prC
--
--   will be of type @Printer (a,(b,c))@, so you may use
--   as its supply function
--
--   > getFinal = getA ## getB ## getC
--
--   which is of type @m (a,(b,c))@.
(##) :: Monad m => m a -> m b -> m (a,b)
(##) = liftM2 (,)

infixr 4 ##

-- | Runs a @dzen@ instance and returns its @stdin@ pipe.
--   Both @stdout@ and @stderr@ of the new process will
--   be the same as this process'. The pipe returned is
--   already line buffered.
--
--   The first string is interpreted as a shell command
--   to start @dzen@. Some examples of usage:
--
--   > createDzen (RawCommand "dzen2" ["-p"])
--   > createDzen (ShellCommand "dzen2 -l 8 -bg #331100")
createDzen :: CmdSpec -> IO Handle
createDzen cmd = createProcess proc >>= extract
    where proc = (shell "") {cmdspec   = cmd
                            ,std_in    = CreatePipe
                            ,std_out   = Inherit
                            ,std_err   = Inherit}
          extract (Just handle, Nothing, Nothing, _) = do
            hSetBuffering handle LineBuffering
            return handle
          extract _ = do
              fail "createDzen: extract: (un)expected pipes"

-- | Like @createDzen@, but never uses a shell (which is good).
createDzen' :: FilePath -- ^ @dzen@ executable, likely @\"dzen2\"@
            -> [String] -- ^ Arguments to @dzen@.
            -> IO Handle
createDzen' = (createDzen .) . RawCommand
