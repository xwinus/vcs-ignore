{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Data.VCS.Ignore.Types
-- Description : Public types shared by repository queries and traversal
-- Copyright   : (c) 2020-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : portable
module Data.VCS.Ignore.Types (
    PathKind (..),
    Entry (..),
    WalkAction (..),
    WalkResult (..),
    GitError (..),
) where

import Control.Exception (Exception (..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Filesystem kind supplied to ignore queries and repository callbacks.
-- Only 'Directory' receives directory-only Git pattern semantics. Symbolic
-- links are never followed, even when their target is a directory.
data PathKind
    = RegularFile
    | Directory
    | SymbolicLink
    | Other
    deriving (Eq, Ord, Show)

-- | A non-ignored repository entry. The path is always relative to the
-- repository root.
--
-- >>> entryPath (Entry "src/Main.hs" RegularFile)
-- "src/Main.hs"
data Entry = Entry
    { entryPath :: FilePath
    , entryKind :: PathKind
    }
    deriving (Eq, Ord, Show)

-- | Controls repository traversal after processing a visible entry.
data WalkAction
    = Continue
    | Prune
    | Stop
    deriving (Eq, Ord, Show)

-- | Indicates whether a repository fold visited all reachable entries or was
-- stopped early by its callback.
data WalkResult a
    = WalkCompleted !a
    | WalkStopped !a
    deriving (Eq, Functor, Show)

-- | Errors caused by invalid Git repository metadata or query paths. Ordinary
-- filesystem failures retain their original 'IOError'.
data GitError
    = NotGitRepository FilePath
    | InvalidRepositoryPath FilePath
    | InvalidGitMetadata FilePath Text
    deriving (Eq, Show)

instance Exception GitError where
    displayException (NotGitRepository path) =
        "Path '" <> path <> "' is not a Git working tree"
    displayException (InvalidRepositoryPath path) =
        "Path '" <> path <> "' is not a valid repository-relative path"
    displayException (InvalidGitMetadata path reason) =
        mconcat ["Invalid Git metadata at '", path, "': ", T.unpack reason]
