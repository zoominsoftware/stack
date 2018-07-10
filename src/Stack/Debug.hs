-- | It's a debugging module, don't look at me like that

module Stack.Debug where

import Stack.Prelude

import System.IO
import System.IO.Unsafe
import Text.Show.Pretty

errorShowPretty :: Show a => a -> b
errorShowPretty a = error (ppShow a)

errorDumpPretty :: Show a => a -> b
errorDumpPretty a =
  unsafePerformIO $ do
    writeFile "errorDumpPretty.txt" (ppShow a)
    error "Dumped the log to errorDumpPretty.txt"
