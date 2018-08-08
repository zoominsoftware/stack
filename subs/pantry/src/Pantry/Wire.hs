{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-- | Wire protocol for pantry's storage server.

module Pantry.Wire
    ( wireReceiverConduit
    , wireSenderConduit
    , wireEventToPairConduit
    , pairToEventConduit
    , WireEvent(..)
    , PantryWireException(..)
    ) where

import           Control.Monad.Catch
import           Control.Monad.Primitive
import qualified Crypto.Hash as Crypto
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Typeable
import           Pantry.StaticSHA256

-- | Thrown by 'wireReceiverConduit'.
data PantryWireException
    = InvalidSha256Input !CA.ParseError
    | UnexpectedSha256 !StaticSHA256 !StaticSHA256
    | TruncatedInputForHash !StaticSHA256 !Int !Int
    | SHA256Mismatch !StaticSHA256 !StaticSHA256
    deriving (Show, Typeable)
instance Exception PantryWireException

-- | A SAX-style event.
data WireEvent
    = Sha256Event {-# UNPACK #-}!StaticSHA256
    | ChunkEvent {-# UNPACK #-}!ByteString

-- | Receive wire events from the pantry server, validating their
-- correctness, checking their lengths and hashes. Throws
-- PantryWireException.
wireReceiverConduit ::
       MonadThrow m
    => [(StaticSHA256, Int)]
    -> ConduitM ByteString WireEvent m ()
wireReceiverConduit expectations = eventConduit
  where
    eventConduit = go expectations
      where
        go [] = pure ()
        go ((expectedSha256, expectedLength):following) = do
            result <- CA.sinkParserEither rawStaticSHA256Parser
            case result of
                Left e -> throwM (InvalidSha256Input e)
                Right givenSha256
                    | givenSha256 == expectedSha256 -> do
                        yield (Sha256Event givenSha256)
                        actualSha256 <-
                            sizeValidatingConduit givenSha256 expectedLength
                        if actualSha256 == givenSha256
                            then go following
                            else throwM
                                     (SHA256Mismatch givenSha256 actualSha256)
                    | otherwise ->
                        throwM (UnexpectedSha256 givenSha256 givenSha256)
    sizeValidatingConduit expectedSha256 expectedLength = do
        (len, ctx) <-
            CB.isolate expectedLength .|
            CL.mapAccum
                (\chunk (!len, ctx) ->
                     ( (len + S.length chunk, Crypto.hashUpdate ctx chunk)
                     , ChunkEvent chunk))
                (0, Crypto.hashInit)
        if len == expectedLength
            then pure (mkStaticSHA256FromDigest (Crypto.hashFinalize ctx))
            else throwM
                     (TruncatedInputForHash expectedSha256 expectedLength len)

-- | Send wire events to the pantry client.
wireSenderConduit ::
       PrimMonad m => ConduitM WireEvent ByteString m ()
wireSenderConduit = CL.map format
  where
    format =
        \case
            Sha256Event sha -> staticSHA256ToRaw sha
            ChunkEvent bs -> bs

-- | A conduit that consumes wire events into (sha,blob) pairs.
wireEventToPairConduit :: Monad m => ConduitM WireEvent (StaticSHA256, ByteString) m ()
wireEventToPairConduit = loop
  where
    loop = do
        msha <- await
        case msha of
            Nothing -> pure ()
            Just (ChunkEvent {}) -> error "Unexpected chunk."
            Just (Sha256Event sha256) -> do
                blob <- consume sha256
                yield blob
                loop
    consume sha256 = fmap (second (L.toStrict . L.toLazyByteString)) (go mempty)
      where
        go acc = do
            mchunk <- await
            case mchunk of
                Just (ChunkEvent chunk) -> go (acc <> L.byteString chunk)
                Just (i@Sha256Event {}) -> do
                    leftover i
                    pure (sha256, acc)
                Nothing -> pure (sha256, acc)

-- | Conduit that puts pairs into wire events.
pairToEventConduit :: Monad m => ConduitM (StaticSHA256, ByteString) WireEvent m ()
pairToEventConduit =
    CL.concatMap (\(sha, bs) -> [Sha256Event sha, ChunkEvent bs])
