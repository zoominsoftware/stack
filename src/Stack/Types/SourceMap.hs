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
  deriving Show

data UnresolvedPackageSource
  = UPSGlobal !Version !GhcPkgId
  | UPSInstallable !UnresolvedInstallable
  deriving Show

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
  deriving Show

data InstallableConfig =
  InstallableConfig {
      installableConfigFlags :: !(Map FlagName Bool)
    , installableConfigGhcOptions :: ![Text]
    -- , installableConfigCompilerVersion :: !(CompilerVersion 'CVActual)
    -- , installableConfigPlatform :: !Platform
  }
  deriving Show

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
  { uiLocation :: !(PackageLocationIndex FilePath)
  , uiInstallableConfig :: !InstallablePackageConfig
  -- , uiPackageConfig :: !PackageConfig -- FIXME probably don't need GHC info in here directly
  }
  deriving Show

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
  deriving Show

type ResolvedSourceMap = Map PackageName ResolvedPackageSourceKey

newtype RPSKey =
  RPSKey { _unRPSKey :: StaticSHA256 } deriving Show

data ResolvedPackageSourceKey =
  RPSK !ResolvedPackageSource !RPSKey
  deriving Show

data ResolvedPackageSource
  = RPSGlobal !Version !GhcPkgId
  | RPSInstallable !ResolvedInstallable
  deriving Show

data ResolvedInstallable = ResolvedInstallable
  { riUnresolved :: !UnresolvedInstallable
  , riDeps :: !(Map PackageName RPSKey)
  }
  deriving Show

data Toolchain =
  Toolchain {
    toolchainCompilerVersion :: CompilerVersion 'CVActual
  -- , toolchainInstallLocation :: Text -- ??? ick
  , toolchainPlatform :: Platform -- ??? ick
  }
  deriving Show
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
  -- let installLocation =
  --       error "TODO: ??? Snapshot?"
  return $ Toolchain cv platform

mkRPSKey
  :: Toolchain
  -> PackageName
  -> ResolvedPackageSource
  -> RPSKey
mkRPSKey toolchain pkgName rps = do
  let toolchainSubkey =
        keyToolchain toolchain
      pkgText =
        packageNameText pkgName
  RPSKey $ buildRps toolchainSubkey pkgText rps
  where
    keyToolchain :: Toolchain -> Text
    keyToolchain (Toolchain compilerVersion platform) =
         compilerVersionText compilerVersion
      <> tshow platform
    buildRps tc pkg (RPSGlobal version ghcPkgId) =
      textToStaticSHA256 $
           tc
        <> pkg
        <> versionText version
        <> ghcPkgIdText ghcPkgId
    buildRps tc pkg (RPSInstallable (ResolvedInstallable unresolved riDeps)) =
      textToStaticSHA256 $ tc <> pkg <> undefined
