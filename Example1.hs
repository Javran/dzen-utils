import Data.Time
import System.Dzen
import System.Locale

printer :: Printer (String, ZonedTime)
printer = str "Wndn: " +++ cstr +++ str " - " +++ simple' format
  where format = formatTime defaultTimeLocale "%a %e %b %H:%M:%S"

supply :: IO (String, ZonedTime)
supply = getCurrentWindow ## getZonedTime

myDzen :: IO ()
myDzen = runDzen "dzen2" [] 500 printer supply

getCurrentWindow :: IO String
getCurrentWindow = return "My =^.^= Lolcats"

main :: IO ()
main = myDzen
