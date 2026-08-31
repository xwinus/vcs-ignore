# vcs-ignore

[![CI](https://github.com/xwinus/vcs-ignore/actions/workflows/ci.yml/badge.svg)](https://github.com/xwinus/vcs-ignore/actions/workflows/ci.yml)
[![Hackage version](https://img.shields.io/hackage/v/vcs-ignore.svg)](https://hackage.haskell.org/package/vcs-ignore)
[![Stackage version](https://www.stackage.org/package/vcs-ignore/badge/lts?label=Stackage)](https://www.stackage.org/package/vcs-ignore)

`vcs-ignore` is a small Haskell library for finding repositories, checking
whether paths are ignored, and processing paths according to version control
system (VCS) ignore rules. Git is currently supported.

## Contents

- [Using the library](#using-the-library)
  - [Pruning a traversal lazily](#pruning-a-traversal-lazily)
  - [Negated patterns and match precedence](#negated-patterns-and-match-precedence)
  - [Listing non-ignored paths](#listing-non-ignored-paths)
  - [Processing non-ignored paths](#processing-non-ignored-paths)
  - [Checking whether a path is ignored](#checking-whether-a-path-is-ignored)
- [Using the executable](#using-the-executable)
  - [Checking whether a path is ignored](#checking-whether-a-path-is-ignored-1)
- [Development](#development)
  - [Running the benchmark](#running-the-benchmark)

## Using the library

The eager examples below use `scanRepo` to scan a Git repository at its root.
Paths returned by `listRepo` and passed to callbacks by `walkRepo` are relative
to that root.

### Pruning a traversal lazily

`GitIgnoreMatcher` supports walkers that need to decide whether a directory is
ignored before visiting its children. Repository discovery does not scan the
working tree. Opening a matcher loads global excludes and `.git/info/exclude`;
each `.gitignore` is then loaded at most once, only when a query needs it.

```haskell
import Data.VCS.Ignore

shouldPrune :: FilePath -> IO Bool
shouldPrune start = do
    maybeRoot <- findRepoRoot start
    case maybeRoot of
        Nothing -> pure False
        Just root -> do
            matcher <- openGitIgnoreMatcher root
            isIgnoredPath matcher DirectoryPathKind "dist"
```

The queried path may be relative to the repository or absolute. Its parent is
canonicalized to verify repository membership, but its final component is
matched lexically. Callers therefore pass `FilePathKind` for files and symlinks,
or `DirectoryPathKind` for real directories, without requiring the matcher to
follow the leaf. A path outside the repository throws `PathOutsideRepository`.

`gitIgnoreMatcherRoot` returns the canonical, absolute repository root that the
matcher resolved when it was opened. Use it to turn the paths a walker produces
into repository-relative ones, or to report where the matcher is anchored.

Missing, unreadable, and symlinked `.gitignore` files are treated as empty and
cached. Git metadata (`.git` and its descendants) is always reported as ignored
for traversal purposes.

The existing eager `scanRepo`, `findRepo`, and `isIgnored` API remains
available. Pattern discovery now also follows Git more closely: a slashless
pattern such as `*.log` applies at every depth (use `/*.log` to anchor it to the
pattern group's root), repository excludes are read from `.git/info/exclude`,
and only files named exactly `.gitignore` are discovered by eager scans.

### Negated patterns and match precedence

`isIgnoredPath` follows Git's precedence rule: every applicable pattern is
tried in order and the **last** one that matches decides the result. A pattern
starting with `!` is a negation, so it re-includes a path that an earlier
pattern ignored.

```gitignore
*.log
!keep.log
```

Under last-match precedence `debug.log` is ignored while `keep.log` is not,
because the negation is the last pattern that matches it. Reversing the two
lines makes both files ignored, exactly as Git would behave.

Two differences apply when you use the eager `isIgnored` instead:

- It does not implement last-match precedence. A path is reported as ignored
  when any non-negated pattern matches and no negated pattern matches, so the
  order of the patterns does not affect the result.
- It takes no `PathKind`, so it cannot tell the matcher whether the path is a
  file or a directory when evaluating directory-only patterns such as `build/`.

Prefer `isIgnoredPath` when you need results that agree with Git.

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
vcs-ignore, v0.0.3.0 :: https://github.com/xwinus/vcs-ignore

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

## Development

### Running the benchmark

The package ships a `lazy-vs-eager` benchmark that compares the cost of an
eager `scanRepo` against opening a lazy matcher and pruning a single ignored
directory. It builds a throwaway repository in a temporary directory with 20
ignored subdirectories holding 200 generated files each, and removes it
afterwards.

```console
$ cabal bench lazy-vs-eager
```

```console
$ stack bench vcs-ignore:bench:lazy-vs-eager
```

A sample run on an Apple M1 Pro with GHC 9.14.1:

| Benchmark             | Mean      |
| --------------------- | --------- |
| `eager-scan`          | 38.03 ms  |
| `lazy-open-and-prune` | 171.5 µs  |

The gap comes from the work each approach does, not from micro-optimisation:
`scanRepo` walks the whole tree and loads every `.gitignore` up front, while
the matcher only reads the repository-wide excludes plus the `.gitignore` files
on the path being queried. Numbers scale with the size of the tree, so treat
them as an illustration of the difference rather than as an absolute figure.
