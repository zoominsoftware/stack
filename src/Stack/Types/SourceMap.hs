{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.SourceMap
  ( -- * Unresolved
    UnresolvedSourceMap
  , UnresolvedPackageSource (..)
  , UnresolvedGlobalPackage (..)
  , UnresolvedInstallable (..)
  , InstallableConfig(..)
  , InstallablePackageConfig(..)
    -- * Loaded
  , LoadedInstallable (..)
    -- * Resolved
  , ResolvedSourceMap
  , RPSKey
  , ResolvedPackageSourceKey (..)
  , ResolvedPackageSource (..)
  , ResolvedInstallable (..)
  , Toolchain (..)
  , getToolchain
  , mkRPSKey
  ) where

import Stack.Prelude

import Distribution.PackageDescription (TestSuiteInterface)
import Distribution.System (Platform(..))
import RIO.Process (HasProcessContext)

import Stack.Setup.Installed
import Stack.Types.BuildPlan
import Stack.Types.Compiler
import Stack.Types.Config
import Stack.Types.FlagName
import Stack.Types.GhcPkgId
import Stack.Types.Package (PackageLibraries)
import Stack.Types.PackageIdentifier
import Stack.Types.PackageName
import Stack.Types.Version

type UnresolvedSourceMap = Map PackageName UnresolvedPackageSource

data UnresolvedGlobalPackage =
  UnresolvedGlobalPackage !Version !GhcPkgId

data UnresolvedPackageSource
  = UPSGlobal !Version !GhcPkgId
  | UPSInstallable !UnresolvedInstallable

--       { packageConfigEnableTests = False
--       , packageConfigEnableBenchmarks = False
--       , packageConfigFlags = flags
--       , packageConfigGhcOptions = options
--       , packageConfigCompilerVersion = compilerVersion
--       , packageConfigPlatform = platform
--       }

-- Overriding flags that apply to any package
--   - snapshot configuration
--   - config files (stack.yaml)
--   - command-line

data InstallablePackageConfig =
    SnapshotPackageConfig InstallableConfig
  -- | LocalCfg UnflaggedInstallableConfig
  | LocalPackageConfig

data InstallableConfig =
  InstallableConfig {
      installableConfigFlags :: !(Map FlagName Bool)
    , installableConfigGhcOptions :: ![Text]
    -- , installableConfigCompilerVersion :: !(CompilerVersion 'CVActual)
    -- , installableConfigPlatform :: !Platform
  }

-- type FlaggedInstallableConfig = InstallableConfig !(Map FlagName Bool) ![Text]
-- type UnflaggedInstallableConfig = InstallableConfig () ()

-- data InstallableConfig flags options =
--   InstallableConfig {
--       installableConfigFlags :: flags -- !(Map FlagName Bool)
--     , installableConfigGhcOptions :: options -- ![Text]
--     , installableConfigCompilerVersion :: !(CompilerVersion 'CVActual)
--     , installableConfigPlatform :: !Platform
--   }

data UnresolvedInstallable = UnresolvedInstallable
  { -- Is this needed?
    -- uiLocation :: !(PackageLocationIndex FilePath),
  uiInstallableConfig :: !InstallablePackageConfig
  -- { uiLocation :: !(PackageLocationIndex FilePath)
  -- , uiPackageConfig :: !PackageConfig -- FIXME probably don't need GHC info in here directly
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

<<<<<<< HEAD
data Toolchain =
  Toolchain {
    toolchainCompilerVersion :: CompilerVersion 'CVActual
  , toolchainInstallLocation :: Text -- ??? ick
  , toolchainPlatform :: Platform -- ??? ick
  }
  -- FIXME identify GHC version, install location, arch, whatever else
  -- we can think of

getToolchain :: ( HasProcessContext env
                , HasLogFunc env
                , HasPlatform env
                )
             => WhichCompiler
             -> RIO env Toolchain
getToolchain wc = do
  cv <- getCompilerVersion wc
  platform <- view platformL
  let installLocation =
        error "TODO: ??? Snapshot?"
  return $ Toolchain cv installLocation platform

mkRPSKey
  :: Toolchain
  -> PackageName
  -> ResolvedPackageSource
<<<<<<< HEAD
  -> Either StringException RPSKey
mkRPSKey toolchain pkgName rps = do
  let toolchainSubkey =
        keyToolchain toolchain
      pkgText =
        packageNameText pkgName
  RPSKey <$> buildRps toolchainSubkey pkgText rps
  where
    keyToolchain :: Toolchain -> Text
    keyToolchain (Toolchain compilerVersion installLocation platform) =
      undefined
    buildRps tc pkg (RPSGlobal version ghcPkgId) =
      mkStaticSHA256FromText $
           tc
        <> pkg
        <> versionText version
        <> ghcPkgIdText ghcPkgId
    buildRps tc pkg (RPSInstallable (ResolvedInstallable unresolved riDeps)) =
      mkStaticSHA256FromText $ tc <> pkg <> undefined
