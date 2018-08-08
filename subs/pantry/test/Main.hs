{-# OPTIONS_GHC -Wall #-}

-- |

module Main where

import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Pantry.StaticSHA256
import           Pantry.Wire

roundTrip :: [(StaticSHA256, ByteString)] -> IO [(StaticSHA256, ByteString)]
roundTrip is =
    runConduit
        (CL.sourceList is .| pairToEventConduit .| wireSenderConduit .|
         wireReceiverConduit expectations .|
         wireEventToPairConduit .|
         CL.consume)
  where
    expectations = map (second S.length) is

main :: IO ()
main = pure ()
