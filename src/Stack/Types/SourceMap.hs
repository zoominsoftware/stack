{-# LANGUAGE NoImplicitPrelude #-}
module Stack.Types.SourceMap
  ( -- * Unresolved
    UnresolvedSourceMap
  , UnresolvedPackageSource (..)
  , UnresolvedInstallable (..)
    -- * Loaded
  , LoadedInstallable (..)
    -- * Resolved
  , ResolvedSourceMap
  , RPSKey
  , ResolvedPackageSourceKey (..)
  , ResolvedPackageSource (..)
  , ResolvedInstallable (..)
  , Toolchain (..)
  -- , mkRPSKey
  ) where

import Distribution.PackageDescription (TestSuiteInterface)
import Stack.Prelude
import Stack.Types.BuildPlan
import Stack.Types.GhcPkgId
import Stack.Types.Package (PackageConfig, PackageLibraries)
import Stack.Types.PackageIdentifier
import Stack.Types.PackageName
import Stack.Types.Version

type UnresolvedSourceMap = Map PackageName UnresolvedPackageSource

data UnresolvedPackageSource
  = UPSGlobal !Version !GhcPkgId
  | UPSInstallable !UnresolvedInstallable

data UnresolvedInstallable = UnresolvedInstallable
  { uiLocation :: !(PackageLocationIndex FilePath)
  , uiPackageConfig :: !PackageConfig -- FIXME probably don't need GHC info in here directly
  }

data LoadedInstallable = LoadedInstallable
  { liUnresolved :: !UnresolvedInstallable
  , liLibraryDeps :: !(Map PackageName VersionRange)
  -- ^ After accounting for flags and what components are enabled,
  -- this is all library dependencies we need, including setup
  -- dependencies. They are also tracked separately in 'liSetupDeps'
  -- for properly building the @Setup.hs@ files.
  , liToolDeps :: !(Map ExeName VersionRange) -- FIXME deal better with Cabal 2.0 stuff?
  , liVersion :: !Version
  , liLibraries :: !PackageLibraries
  , liInternalLibraries :: !(Set Text)
  , liTests :: !(Map Text TestSuiteInterface)
  , liBenchmarks :: !(Set Text)
  , liExes :: !(Set ExeName)
  , liSetupDeps :: !(Maybe (Map PackageName VersionRange))
  }

type ResolvedSourceMap = Map PackageName ResolvedPackageSourceKey

newtype RPSKey = RPSKey { _unRPSKey :: StaticSHA256 }

data ResolvedPackageSourceKey = RPSK !ResolvedPackageSource !RPSKey

data ResolvedPackageSource
  = RPSGlobal !Version !GhcPkgId
  | RPSInstallable !ResolvedInstallable

data ResolvedInstallable = ResolvedInstallable
  { riUnresolved :: !UnresolvedInstallable
  , riDeps :: !(Map PackageName RPSKey)
  }

data Toolchain = Toolchain
  -- FIXME identify GHC version, install location, arch, whatever else
  -- we can think of

-- mkRPSKey
--   :: Toolchain
--   -> PackageName
--   -> ResolvedPackageSource
--   -> RPSKey
-- mkRPSKey _ _ _ = RPSKey $ error "mkRPSKey"
