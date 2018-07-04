{-# LANGUAGE NoImplicitPrelude #-}
module Stack.SourceMap
  (
  ) where

import Stack.Prelude
import Stack.Types.SourceMap

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
