{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.SourceMap
  ( getUnresolvedSourceMap
  , resolveSourceMap
  ) where

import qualified Data.Map as Map
import RIO.Process (HasProcessContext)

import Stack.Config
import Stack.Prelude
import Stack.Types.BuildPlan
import Stack.Types.Compiler
import Stack.Types.Config
import Stack.Types.GhcPkgId
import Stack.Types.PackageName
import Stack.Types.SourceMap


-- getGlobalPackages :: ( HasLogFunc env
--                      , HasProcessContext env
--                      )
--                   => RIO env (Map PackageName UnresolvedGlobalPackage)
-- getGlobalPackages =
--   ghcPkgDump Ghc [] (conduitDumpPackage .| CL.foldMap f)
--   where f :: DumpPackage () () ()
--           -> Map PackageName UnresolvedGlobalPackage
--         f DumpPackage{..} = Map.singleton packageName unresolvedPkg
--           where packageName = packageIdentifierName dpPackageIdent
--                 packageVersion = packageIdentifierVersion dpPackageIdent
--                 unresolvedPkg =
--                   UnresolvedGlobalPackage
--                     packageVersion
--                     dpGhcPkgId

-- getInstallablePackages :: ( HasEnvConfig env
--                           , HasLogFunc env
--                           , HasProcessContext env
--                           )
--                        => NeedTargets
--                        -> BuildOptsCLI
--                        -> RIO env (Map PackageName UnresolvedInstallable)
-- getInstallablePackages needTargets boptsCli = do
  -- getLocalPackages
  -- let goLPI loc n lpi = do
  --       let configOpts = getGhcOptions bconfig boptsCli n False False
  --       case lpiLocation lpi of
  --         -- NOTE: configOpts includes lpiGhcOptions for now, this may get refactored soon
  --         PLIndex pir -> return $ PSIndex loc (lpiFlags lpi) configOpts pir
  --         PLOther pl -> do
  --           root <- view projectRootL
            -- lpv <- parseSingleCabalFile root True pl
            -- lp' <- loadLocalPackage False boptsCli targets (n, lpv)
  --           return $ PSFiles lp' loc
  -- (ls, localDeps, targets) <- parseTargets needTargets boptsCli

resolveSourceMap :: forall env
                  . ( HasEnvConfig env
                    )
                 => UnresolvedSourceMap
                 -> RIO env ResolvedSourceMap
resolveSourceMap usm = do
  -- TODO: Force Ghc?
  toolchain <- getToolchain Ghc
  undefined $ Map.foldlWithKey' (f toolchain) (return Map.empty) usm
  where f :: Toolchain
          -> (RIO env ResolvedSourceMap)
          -> PackageName
          -> UnresolvedPackageSource
          -> RIO env ResolvedSourceMap
        f toolchain rsmM pkgName (UPSGlobal version ghcPkgId) = do
          rsm <- rsmM
          let rps = undefined
              rpsKeyE =
                mkRPSKey toolchain pkgName rps
          case rpsKeyE of
              (Left err) -> throwM err
              (Right rpsKey) -> do
                return $
                  Map.insert
                    pkgName
                    (RPSK (RPSGlobal version ghcPkgId) rpsKey)
                    rsm
        f toolchain rsm pkgName (UPSInstallable (UnresolvedInstallable ipc)) =
          case ipc of
            LocalPackageConfig ->
              undefined -- ???
            SnapshotPackageConfig InstallableConfig{..} -> do
              -- installableConfigFlags
              -- installableGhcOptions
              undefined

getUnresolvedSourceMap :: ( HasEnvConfig env
                          , HasLogFunc env
                          , HasProcessContext env
                          )
                       => RIO env UnresolvedSourceMap
getUnresolvedSourceMap = do
  loadedSnapshot <- view loadedSnapshotL
  localPackages <- getLocalPackages
  let globals = lsGlobals loadedSnapshot
      snapshotGlobalToUnresolved :: LoadedPackageInfo GhcPkgId -> UnresolvedPackageSource
      snapshotGlobalToUnresolved LoadedPackageInfo{..} =
        UPSGlobal lpiVersion lpiLocation
      globalMap :: UnresolvedSourceMap
      globalMap = fmap snapshotGlobalToUnresolved globals
  installableConfigSnapshotPackages <-
    getInstallableConfigForSnapshot loadedSnapshot
  let installableConfigLocalPackages =
        getInstallableConfigForLocalPackages localPackages
      unresolvedSnapLocalPackages :: UnresolvedSourceMap
      unresolvedSnapLocalPackages =
        fmap (UPSInstallable . UnresolvedInstallable) $
        -- Local packages shadow snapshot packages, union prioritizes left val 
        Map.union installableConfigLocalPackages installableConfigSnapshotPackages
  -- the local-prioritized union of snapshot & local packages with global packages,
  -- with more-local taking priority over global
  return $ Map.union unresolvedSnapLocalPackages globalMap

-- UnresolvedSourceMap -> ResolvedSourceMap

data GetInstallableConfigError =
  CouldntFindLoadedPackageInfoInSnapshot PackageName
  deriving Show

getInstallableConfigForLocalPackages
  :: LocalPackages
  -> Map PackageName InstallablePackageConfig
getInstallableConfigForLocalPackages localPackages =
  fmap (const LocalPackageConfig) (lpProject localPackages)

getInstallableConfigForSnapshot
  :: ( HasPlatform s
     , MonadReader s m
     )
  => LoadedSnapshot
  -> m (Map PackageName InstallablePackageConfig)
getInstallableConfigForSnapshot loadedSnapshot = do
  -- platform <- view platformL
  return $ fmap f (lsPackages loadedSnapshot)
  where f :: (LoadedPackageInfo (PackageLocationIndex FilePath))
          -> InstallablePackageConfig
        f lpi = SnapshotPackageConfig $
          InstallableConfig {
              installableConfigFlags = lpiFlags lpi
            , installableConfigGhcOptions = lpiGhcOptions lpi
          }

  -- let maybeLpi :: Maybe (LoadedPackageInfo (PackageLocationIndex FilePath))
  --     maybeLpi = Map.lookup packageName (lsPackages loadedSnapshot)
  --     promotedLpi ::
  --       Either
  --         GetInstallableConfigError
  --         (LoadedPackageInfo (PackageLocationIndex FilePath))
  --     promotedLpi =
  --       maybe (Left (CouldntFindLoadedPackageInfoInSnapshot packageName)) Right maybeLpi
  -- case promotedLpi of
  --   (Left err) ->
  --     return $ Left err
  --   (Right lpi) -> do
  --     return $ Right $ InstallableConfig {
  --                           installableConfigFlags = lpiFlags lpi
  --                         , installableConfigGhcOptions = lpiGhcOptions lpi
  --                         -- , installableConfigCompilerVersion = lsCompilerVersion loadedSnapshot
  --                         -- , installableConfigPlatform = platform
  --                       }
 
-- -- | Calculate a 'LoadedPackageInfo' from the given 'GenericPackageDescription'
-- calculate :: GenericPackageDescription
--           -> Platform
--           -> CompilerVersion 'CVActual
--           -> loc
--           -> Map FlagName Bool
--           -> Bool -- ^ hidden?
--           -> [Text] -- ^ GHC options
--           -> (PackageName, LoadedPackageInfo loc)
-- calculate gpd platform compilerVersion loc flags hide options =
--     (name, lpi)
--   where
--     pconfig = PackageConfig
--       { packageConfigEnableTests = False
--       , packageConfigEnableBenchmarks = False
--       , packageConfigFlags = flags
--       , packageConfigGhcOptions = options
--       , packageConfigCompilerVersion = compilerVersion
--       , packageConfigPlatform = platform
--       }
--     -- We want to ignore test suites and benchmarks, therefore choose
--     -- the package description which modifies buildable
--     pd = pdpModifiedBuildable $ resolvePackageDescription pconfig gpd
--     PackageIdentifier name version = fromCabalPackageIdentifier $ C.package pd
--     lpi = LoadedPackageInfo
--       { lpiVersion = version
--       , lpiLocation = loc
--       , lpiFlags = flags
--       , lpiGhcOptions = options
--       , lpiPackageDeps = Map.map fromVersionRange
--                        $ Map.filterWithKey (const . (/= name))
--                        $ packageDependencies pconfig pd
--       , lpiProvidedExes =
--             Set.fromList
--           $ map (ExeName . T.pack . C.unUnqualComponentName . C.exeName)
--           $ C.executables pd
--       , lpiNeededExes = Map.map fromVersionRange
--                       $ packageDescTools pd
--       , lpiExposedModules = maybe
--           Set.empty
--           (Set.fromList . map fromCabalModuleName . C.exposedModules)
--           (C.library pd)
--       , lpiHide = hide
--       }

-- getGlobalPackages :: ( HasLogFunc env
--                      , HasProcessContext env
--                      )
--                   => RIO env (Map PackageName UnresolvedGlobalPackage)
--                   -- [(PackageName, (!Version, !GhcPkgId))]
-- getGlobalPackages = do
--   dumpPackages <- getGlobalPackages'
--   foldl' foldDumpPackages Map.empty
--   where foldDumpPackages :: 
--                          -> Map PackageName UnresolvedGlobalPackage
--                          -> Map PackageName UnresolvedGlobalPackage

-- constructPlan
--   :: LoadedSnapshot
--   -> BaseConfigOpts
--   -> [LocalPackage]
--   -> Set PackageName
--   -> [Stack.PackageDump.DumpPackage () () ()]
--   -> (PackageLocationIndex FilePath -> Map FlagName Bool -> [Text] -> RIO env0 Package)
--   -> SourceMap
--   -> InstalledMap
--   -> Bool
--   -> RIO env Plan
-- constructPlan = undefined

-- makeSourceMap :: ??? -> RIO env UnresolvedSourceMap
-- We already have code that's building a source map elsewhere, a sensible route forward may be to copy that code, and then change it to work with the new data types
-- it's called SourceMap
-- Stack.Types.Package
-- Stack.Build.Source

-- loadSourceMapFull :: HasEnvConfig env
--                   => NeedTargets
--                   -> BuildOptsCLI
--                   -> RIO env
--                        ( Map PackageName Target
--                        , LoadedSnapshot
--                        , [LocalPackage] -- FIXME do we really want this? it's in the SourceMap
--                        , Set PackageName -- non-project targets
--                        , SourceMap
--                        )


-- After you have a SourceMap, you'll look at which packages are targets of the current build, discover their dependencies, and get a set of packages we want

-- Then get the hashes for all of these packages, and figure out which ones already exist, and which are already in the relevant package database (I'm a bit confused on this point still, to be followed up on later)

-- And then figure out the order in which to build packages, and when to run test suites/benchmarks

-- Yeah, the unresolved vs resolved source map thing is something I'm not quite sure how to break down, it depends on when we parse the cabal files and figure out the build tools

-- IIRC, the last thing I'd figured out is that we _must_ parse the cabal files when we make the unresolved source map and fill in the executables it provides, and then we can use that info to properly resolve the source map

-- Reason executables are important:
-- If your cabal file says that it depends on the build tool `alex`, what package does that come from?
-- We figure it out by finding the package providing that exe, which happens to be `alex`
-- But other build tools come from packages with different names
-- This is a really ugly part of cabal, and arguably we could look at the changes they made in cabal-install 2.0 that simplified how this lookup happens
-- I think they essentially hard-coded a few legacy names, and demanded that all new stuff be fully resolved with a package name

-- alex
-- happy
-- doctests
-- hspec-discover
-- code formatters
