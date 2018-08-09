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
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

main :: IO ()
main =
    hspec
        (describe
             "Wire protocol"
             (it "Roundtrip"
                  (property
                       (\wrds ->
                            monadicIO
                                (do let in' =
                                            [ ( mkStaticSHA256FromBytes bytes
                                              , bytes)
                                            | bytes <- map S.pack wrds
                                            ]
                                    out <- run (roundTrip in')
                                    pure (out == in'))))))

roundTrip :: [(StaticSHA256, ByteString)] -> IO [(StaticSHA256, ByteString)]
roundTrip is =
    runConduit
        (CL.sourceList is .| pairToEventConduit .| wireSenderConduit .|
         wireReceiverConduit expectations .|
         wireEventToPairConduit .|
         CL.consume)
  where
    expectations = map (second S.length) is
