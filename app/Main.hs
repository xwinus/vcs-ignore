{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Main
-- Description : Simple application using the /vcs-ignore/ library
-- Copyright   : (c) 2020-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This simple application demonstrates the use of "vcs-ignore" library. It allows
-- to check whether path given as argument is ignored within existing /GIT/ repo.
module Main (
    main,
) where

import Control.Monad (when)
import Data.VCS.Ignore (
    GitRepository,
    PathKind (..),
    findGitRepository,
    isIgnored,
    repositoryRoot,
 )
import Main.Options (
    Mode (..),
    Options (..),
    optionsParser,
 )
import Options.Applicative (execParser)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    makeAbsolute,
    pathIsSymbolicLink,
 )
import System.Exit (
    exitFailure,
    exitSuccess,
 )
import System.FilePath (makeRelative, normalise)
import System.IO.Error (
    isDoesNotExistError,
    tryIOError,
 )

main :: IO ()
main = do
    options <- execParser optionsParser
    repo <- findRepoOrFail options
    executeMode repo options

findRepoOrFail :: Options -> IO GitRepository
findRepoOrFail Options{..} = do
    repoDir <- getCurrentDirectory
    maybeRepo <- findGitRepository repoDir
    case maybeRepo of
        Just repo -> do
            putStrLn $ "Found repository at: " <> repositoryRoot repo
            when oDebug (putStrLn $ "Repository root: " <> repositoryRoot repo)
            pure repo
        Nothing -> do
            putStrLn $ "No repository found for path: " <> repoDir
            exitFailure

executeMode :: GitRepository -> Options -> IO ()
executeMode repo (oMode -> Path path) = checkPath repo path

checkPath :: GitRepository -> FilePath -> IO ()
checkPath repo path = do
    absolute <- normalise <$> makeAbsolute path
    kind <- classifyPath absolute
    let relative = makeRelative (repositoryRoot repo) absolute
    excluded <- isIgnored repo kind relative
    if excluded then reportIgnored else reportNotIgnored
  where
    reportIgnored = do
        putStrLn $ "Path '" <> path <> "' IS ignored"
        exitSuccess
    reportNotIgnored = do
        putStrLn $ "Path '" <> path <> "' IS NOT ignored"
        exitFailure

classifyPath :: FilePath -> IO PathKind
classifyPath path = do
    symbolicLinkResult <- tryIOError $ pathIsSymbolicLink path
    case symbolicLinkResult of
        Left error'
            | isDoesNotExistError error' -> pure Other
            | otherwise -> ioError error'
        Right True -> pure SymbolicLink
        Right False -> do
            directory <- doesDirectoryExist path
            regularFile <- doesFileExist path
            pure $ if directory then Directory else if regularFile then RegularFile else Other
