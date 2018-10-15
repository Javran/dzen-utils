import Data.Monoid
import Data.Time
import System.Dzen
import System.Locale

sep :: DString
sep = mconcat [str " ", rectO 5 5, str " "]

timeBar :: Printer Int
timeBar = cgdbar False (40,10) Nothing Nothing True (0, 60*60-1)

zonedSecs :: ZonedTime -> Int
zonedSecs = extract . localTimeOfDay . zonedTimeToLocalTime
    where extract t = let minutes = fromIntegral (todMin t)
                      in round (minutes * 60 + todSec t)

timeBar' :: Printer ZonedTime
timeBar' = comap zonedSecs timeBar

time :: Printer ZonedTime
time = simple' format +=+ timeBar'
  where format = formatTime defaultTimeLocale "%a %e %b %H:"

printer :: Printer (String, ZonedTime)
printer = str "Wndn: " +++ cstr +++ sep +++ time

supply :: IO (String, ZonedTime)
supply = getCurrentWindow ## getZonedTime

myDzen :: IO ()
myDzen = runDzen "dzen2" [] 500 printer supply

getCurrentWindow :: IO String
getCurrentWindow = return "My =^.^= Lolcats"

main :: IO ()
main = myDzen
