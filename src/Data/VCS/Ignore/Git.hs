-- |
-- Module      : Data.VCS.Ignore.Git
-- Description : Lazy Git ignore queries and repository traversal
-- Copyright   : (c) 2020-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclavsvejcar@gmail.com
-- Stability   : experimental
-- Portability : portable
module Data.VCS.Ignore.Git (
    GitRepository,
    PathKind (..),
    Entry (..),
    WalkAction (..),
    WalkResult (..),
    GitError (..),
    openGitRepository,
    findGitRepository,
    repositoryRoot,
    isIgnored,
    foldRepo,
    walkRepo,
    forRepo_,
    listRepo,
) where

import Data.VCS.Ignore.Git.Internal.Repository (
    GitRepository,
    findGitRepository,
    isIgnored,
    openGitRepository,
    repositoryRoot,
 )
import Data.VCS.Ignore.Git.Internal.Traversal (
    foldRepo,
    forRepo_,
    listRepo,
    walkRepo,
 )
import Data.VCS.Ignore.Types (
    Entry (..),
    GitError (..),
    PathKind (..),
    WalkAction (..),
    WalkResult (..),
 )
