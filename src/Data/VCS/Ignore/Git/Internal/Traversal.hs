{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.VCS.Ignore.Git.Internal.Traversal
-- Description : Pruning, early-stopping traversal of a Git working tree
-- Copyright   : (c) 2020-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : portable
module Data.VCS.Ignore.Git.Internal.Traversal (
    foldRepo,
    walkRepo,
    forRepo_,
    listRepo,
) where

import Control.Monad (void)
import Data.VCS.Ignore.Git.Internal.Pattern (
    PatternGroup,
    evaluatePatternGroups,
 )
import Data.VCS.Ignore.Git.Internal.Repository (
    GitRepository,
    isRepositoryMetadata,
    loadDirectoryRules,
    repositoryRoot,
    rootRuleContext,
 )
import Data.VCS.Ignore.Types (
    Entry (..),
    PathKind (..),
    WalkAction (..),
    WalkResult (..),
 )
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectory,
    pathIsSymbolicLink,
 )
import System.FilePath ((</>))
import System.IO.Error (
    isDoesNotExistError,
    tryIOError,
 )

data WorkItem
    = VisitDirectory [PatternGroup] FilePath
    | VisitEntry [PatternGroup] FilePath

-- | Folds visible repository entries in depth-first preorder.
--
-- The repository root is not passed to the callback. Ignored directories and
-- repository metadata are pruned before the callback is invoked. A callback
-- can prune any other directory or stop the complete traversal. Symbolic links
-- are emitted as links and are never followed. Sibling order is the order
-- supplied by the filesystem and is intentionally unspecified.
foldRepo ::
    GitRepository ->
    state ->
    (state -> Entry -> IO (state, WalkAction)) ->
    IO (WalkResult state)
foldRepo repository initialState step = do
    initialState `seq` pure ()
    rules <- rootRuleContext repository
    loop initialState [VisitDirectory rules ""]
  where
    loop !state [] = pure $ WalkCompleted state
    loop !state (VisitDirectory rules relativeDirectory : pending) = do
        namesResult <- tryIOError . listDirectory $ absolutePath relativeDirectory
        case namesResult of
            Left error'
                | isDoesNotExistError error' -> loop state pending
                | otherwise -> ioError error'
            Right names -> do
                let entries = VisitEntry rules . childPath relativeDirectory <$> names
                loop state (entries <> pending)
    loop !state (VisitEntry rules relative : pending) =
        if isRepositoryMetadata repository relative
            then loop state pending
            else do
                maybeKind <- classifyPath $ absolutePath relative
                case maybeKind of
                    Nothing -> loop state pending
                    Just kind ->
                        if evaluatePatternGroups kind rules relative
                            then loop state pending
                            else do
                                (!nextState, action) <- step state $ Entry relative kind
                                continueFrom rules pending relative kind nextState action

    continueFrom _ _ _ _ !state Stop = pure $ WalkStopped state
    continueFrom rules pending relative Directory !state Continue = do
        currentKind <- classifyPath $ absolutePath relative
        case currentKind of
            Just Directory -> do
                directoryRules <- loadDirectoryRules repository relative
                loop state $ VisitDirectory (rules <> [directoryRules]) relative : pending
            _ -> loop state pending
    continueFrom _ pending _ _ !state _ = loop state pending

    absolutePath "" = repositoryRoot repository
    absolutePath relative = repositoryRoot repository </> relative

-- | Walks visible entries for their effects and traversal control.
walkRepo ::
    GitRepository ->
    (Entry -> IO WalkAction) ->
    IO (WalkResult ())
walkRepo repository action = foldRepo repository () step
  where
    step () entry = do
        nextAction <- action entry
        pure ((), nextAction)

-- | Performs an action for every visible entry.
forRepo_ :: GitRepository -> (Entry -> IO ()) -> IO ()
forRepo_ repository action =
    void (walkRepo repository $ \entry -> action entry >> pure Continue)

-- | Lists all visible repository entries in traversal order.
listRepo :: GitRepository -> IO [Entry]
listRepo repository = do
    result <- foldRepo repository [] collect
    pure . reverse $ case result of
        WalkCompleted entries -> entries
        WalkStopped entries -> entries
  where
    collect entries entry = pure (entry : entries, Continue)

childPath :: FilePath -> FilePath -> FilePath
childPath "" name = name
childPath parent name = parent </> name

-- A path may disappear after its name was returned by 'listDirectory'. Such an
-- entry is skipped, while all other I/O errors retain their original failure.
classifyPath :: FilePath -> IO (Maybe PathKind)
classifyPath path = do
    symbolicLinkResult <- tryIOError $ pathIsSymbolicLink path
    case symbolicLinkResult of
        Left error'
            | isDoesNotExistError error' -> pure Nothing
            | otherwise -> ioError error'
        Right True -> pure $ Just SymbolicLink
        Right False -> classifyNonLink
  where
    classifyNonLink = do
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then pure $ Just Directory
            else do
                isFile <- doesFileExist path
                if isFile
                    then pure $ Just RegularFile
                    else do
                        exists <- doesPathExist path
                        pure $ if exists then Just Other else Nothing
