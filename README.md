# vcs-ignore

[![CI](https://github.com/xwinus/vcs-ignore/actions/workflows/ci.yml/badge.svg)](https://github.com/xwinus/vcs-ignore/actions/workflows/ci.yml)
[![Hackage version](https://img.shields.io/hackage/v/vcs-ignore.svg)](https://hackage.haskell.org/package/vcs-ignore)
[![Stackage version](https://www.stackage.org/package/vcs-ignore/badge/lts?label=Stackage)](https://www.stackage.org/package/vcs-ignore)

`vcs-ignore` is a small Haskell library for finding repositories, checking
whether paths are ignored, and processing paths according to version control
system (VCS) ignore rules. Git is currently supported.

## Contents

- [Using the library](#using-the-library)
  - [Listing non-ignored paths](#listing-non-ignored-paths)
  - [Processing non-ignored paths](#processing-non-ignored-paths)
  - [Checking whether a path is ignored](#checking-whether-a-path-is-ignored)
- [Using the executable](#using-the-executable)
  - [Checking whether a path is ignored](#checking-whether-a-path-is-ignored-1)

## Using the library

The examples below use `scanRepo` to scan a Git repository at its root. Paths
returned by `listRepo` and passed to callbacks by `walkRepo` are relative to that
root.

### Listing non-ignored paths

`listRepo` recursively lists files and directories that are not ignored by the
repository's ignore rules.

```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore (Git, Repo (..), listRepo)

listNonIgnoredPaths :: IO [FilePath]
listNonIgnoredPaths = do
    repo <- scanRepo @Git "/path/to/repo"
    listRepo repo
```

### Processing non-ignored paths

`walkRepo` runs an action for every non-ignored path. This example keeps only
files and returns their paths relative to the repository root.

```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.Maybe (catMaybes)
import Data.VCS.Ignore (Git, Repo (..), walkRepo)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

onlyFiles :: IO [FilePath]
onlyFiles = do
    repo <- scanRepo @Git "/path/to/repo"
    catMaybes <$> walkRepo repo (keepFile repo)
  where
    keepFile repo path = do
        isFile <- doesFileExist (repoRoot repo </> path)
        pure (if isFile then Just path else Nothing)
```

### Checking whether a path is ignored

`isIgnored` expects a path relative to the repository root. The path does not
need to exist.

```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore (Git, Repo (..))

checkIgnored :: IO Bool
checkIgnored = do
    repo <- scanRepo @Git "/path/to/repo"
    isIgnored repo "some/path/.DS_Store"
```

## Using the executable

The package also includes an executable named `ignore`. Run it from anywhere
inside a Git repository to check a path against that repository's ignore rules.

```console
$ ignore --help
vcs-ignore, v0.0.3.0 :: https://github.com/vaclavsvejcar/vcs-ignore

Usage: ignore (-p|--path PATH) [--debug] [-v|--version] [--numeric-version]

  library for handling files ignored by VCS systems

Available options:
  -p,--path PATH           path to check
  --debug                  produce more verbose output
  -v,--version             show version info
  --numeric-version        show only version number
  -h,--help                Show this help text
```

### Checking whether a path is ignored

Pass the path to `--path` (or `-p`). The executable exits with status `0` when
the path is ignored and status `1` when it is not ignored. It also exits with
status `1` if no Git repository can be found.

```console
$ ignore -p foo/.DS_Store
Found repository at: /path/to/repo
Path 'foo/.DS_Store' IS ignored

$ echo $?
0

$ ignore -p README.md
Found repository at: /path/to/repo
Path 'README.md' IS NOT ignored

$ echo $?
1
```
