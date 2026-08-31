{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Data.VCS.Ignore.Repo.Git.Matcher
-- Description : Lazy, path-preserving Git ignore matcher
-- Copyright   : (c) 2020-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : portable
module Data.VCS.Ignore.Repo.Git.Matcher (
    GitIgnoreMatcher,
    GitIgnoreMatcherError (..),
    PathKind (..),
    findRepoRoot,
    openGitIgnoreMatcher,
    gitIgnoreMatcherRoot,
    isIgnoredPath,
) where

import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    newMVar,
 )
import Control.Exception (
    Exception (..),
    IOException,
    catch,
 )
import Control.Monad.Catch (
    MonadThrow,
    throwM,
 )
import Control.Monad.IO.Class (
    MonadIO,
    liftIO,
 )
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.VCS.Ignore.Repo (RepoError (..))
import Data.VCS.Ignore.Repo.Git (
    PathKind (..),
    Pattern,
    evaluatePatternGroups,
    globalPatterns,
    loadPatterns,
 )
import Data.VCS.Ignore.Types (
    fromVCSIgnoreError,
    toVCSIgnoreError,
 )
import System.Directory (
    canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    makeAbsolute,
    pathIsSymbolicLink,
 )
import System.FilePath (
    isAbsolute,
    makeRelative,
    normalise,
    pathSeparator,
    splitDirectories,
    takeDirectory,
    takeFileName,
    (</>),
 )

-- | A matcher whose repository-wide rules are loaded when it is opened and
-- whose @.gitignore@ rules are loaded and cached on demand.
data GitIgnoreMatcher = GitIgnoreMatcher
    { matcherRoot :: FilePath
    , matcherBasePatterns :: [Pattern]
    , matcherPatternCache :: MVar (M.Map FilePath [Pattern])
    }

-- | Errors specific to lazy Git ignore matching.
data GitIgnoreMatcherError
    = PathOutsideRepository FilePath FilePath
    deriving (Eq, Show)

instance Exception GitIgnoreMatcherError where
    displayException (PathOutsideRepository root path) =
        mconcat ["Path '", path, "' is outside repository '", root, "'"]
    fromException = fromVCSIgnoreError
    toException = toVCSIgnoreError

-- | Finds the nearest enclosing Git repository without scanning its contents.
findRepoRoot :: (MonadIO m) => FilePath -> m (Maybe FilePath)
findRepoRoot path = liftIO $ do
    absolute <- makeAbsolute path
    isDirectory <- doesDirectoryExist absolute
    start <- canonicalizePath $ if isDirectory then absolute else takeDirectory absolute
    go start
  where
    go directory = do
        isRepository <- hasGitMetadata directory
        let parent = takeDirectory directory
        if isRepository
            then pure $ Just directory
            else
                if parent == directory
                    then pure Nothing
                    else go parent

-- | Opens a matcher for a Git repository root. Global excludes and the
-- repository's @info/exclude@ are loaded immediately. Missing or unreadable
-- exclude files are treated as empty, consistently with 'loadPatterns'.
openGitIgnoreMatcher ::
    (MonadIO m, MonadThrow m) =>
    FilePath ->
    m GitIgnoreMatcher
openGitIgnoreMatcher path = do
    root <- liftIO $ makeAbsolute path >>= canonicalizePath
    maybeGitDirectory <- liftIO $ resolveGitDirectory root
    case maybeGitDirectory of
        Nothing -> throwM $ InvalidRepo root "not a valid GIT repository"
        Just gitDirectory -> do
            global <- globalPatterns
            repository <- loadPatterns $ gitDirectory </> "info" </> "exclude"
            cache <- liftIO $ newMVar M.empty
            pure
                GitIgnoreMatcher
                    { matcherRoot = root
                    , matcherBasePatterns = global <> repository
                    , matcherPatternCache = cache
                    }

-- | Returns the canonical absolute repository root used by the matcher.
gitIgnoreMatcherRoot :: GitIgnoreMatcher -> FilePath
gitIgnoreMatcherRoot = matcherRoot

-- | Checks a repository-relative or absolute path against Git ignore rules.
-- Parent directories are canonicalized to establish repository membership,
-- while the final component is retained lexically so file symlinks are matched
-- by their repository name. Paths outside the repository throw
-- 'PathOutsideRepository'.
isIgnoredPath ::
    (MonadIO m, MonadThrow m) =>
    GitIgnoreMatcher ->
    PathKind ->
    FilePath ->
    m Bool
isIgnoredPath matcher kind path = do
    relative <- resolveCandidate matcher path
    if relative == "."
        then pure False
        else
            if isGitMetadata relative
                then pure True
                else do
                    groups <- loadAncestorGroups matcher relative
                    pure $ evaluatePatternGroups kind groups (toPosix relative)

resolveCandidate ::
    (MonadIO m, MonadThrow m) =>
    GitIgnoreMatcher ->
    FilePath ->
    m FilePath
resolveCandidate matcher path = do
    let root = matcherRoot matcher
        candidate = normalise $ if isAbsolute path then path else root </> path
    lexical <-
        liftIO $
            if candidate == root
                then pure root
                else do
                    parent <- canonicalizePath $ takeDirectory candidate
                    pure . normalise $ parent </> takeFileName candidate
    let relative = makeRelative root lexical
    if isOutside relative
        then throwM $ PathOutsideRepository root path
        else pure relative

loadAncestorGroups ::
    (MonadIO m) =>
    GitIgnoreMatcher ->
    FilePath ->
    m [(FilePath, [Pattern])]
loadAncestorGroups matcher relative =
    liftIO (modifyMVar (matcherPatternCache matcher) updateCache)
  where
    updateCache cache = do
        (updatedCache, groups) <- go cache baseGroups (ancestorPrefixes relative)
        pure (updatedCache, groups)
    baseGroups = [("/", matcherBasePatterns matcher)]
    go cache groups [] = pure (cache, groups)
    go cache groups (prefix : prefixes)
        | prefix /= "/"
            && evaluatePatternGroups DirectoryPathKind groups (directoryPath prefix) =
            pure (cache, groups)
        | otherwise = do
            (updatedCache, patterns) <- cachedPatterns cache prefix
            go updatedCache (groups <> [(prefix, patterns)]) prefixes
    cachedPatterns cache prefix = case M.lookup prefix cache of
        Just patterns -> pure (cache, patterns)
        Nothing -> do
            patterns <- loadGitIgnore $ ignoreFilePath (matcherRoot matcher) prefix
            pure (M.insert prefix patterns cache, patterns)

loadGitIgnore :: FilePath -> IO [Pattern]
loadGitIgnore path = catch load onError
  where
    load = do
        isSymbolicLink <- pathIsSymbolicLink path
        if isSymbolicLink then pure [] else loadPatterns path
    onError (_ :: IOException) = pure []

ancestorPrefixes :: FilePath -> [FilePath]
ancestorPrefixes relative =
    scanl appendSegment "/" directorySegments
  where
    directory = takeDirectory relative
    directorySegments
        | directory == "." = []
        | otherwise = filter (`notElem` [".", ""]) $ splitDirectories directory
    appendSegment "/" segment = "/" <> toPosix segment <> "/"
    appendSegment prefix segment = prefix <> toPosix segment <> "/"

ignoreFilePath :: FilePath -> FilePath -> FilePath
ignoreFilePath root prefix =
    root </> fromPosix (stripSlashes prefix) </> ".gitignore"

directoryPath :: FilePath -> FilePath
directoryPath = stripTrailingSlash

isGitMetadata :: FilePath -> Bool
isGitMetadata relative = case splitDirectories relative of
    ".git" : _ -> True
    _ -> False

isOutside :: FilePath -> Bool
isOutside relative =
    isAbsolute relative
        || case splitDirectories relative of
            ".." : _ -> True
            _ -> False

hasGitMetadata :: FilePath -> IO Bool
hasGitMetadata root = do
    let path = root </> ".git"
    isDirectory <- doesDirectoryExist path
    isFile <- doesFileExist path
    pure $ isDirectory || isFile

resolveGitDirectory :: FilePath -> IO (Maybe FilePath)
resolveGitDirectory root = do
    let metadata = root </> ".git"
    isDirectory <- doesDirectoryExist metadata
    if isDirectory
        then Just <$> resolveCommonDirectory metadata
        else do
            isFile <- doesFileExist metadata
            if isFile then resolveGitFile root metadata else pure Nothing

resolveGitFile :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveGitFile root metadata = do
    content <- readFirstLine metadata
    case T.stripPrefix "gitdir:" content of
        Nothing -> pure Nothing
        Just rawPath -> do
            let gitPath = T.unpack . T.strip $ rawPath
                resolved = if isAbsolute gitPath then gitPath else root </> gitPath
            exists <- doesDirectoryExist resolved
            if exists
                then Just <$> resolveCommonDirectory resolved
                else pure Nothing

resolveCommonDirectory :: FilePath -> IO FilePath
resolveCommonDirectory gitDirectory = do
    let commonFile = gitDirectory </> "commondir"
    exists <- doesFileExist commonFile
    if exists
        then do
            commonPath <- T.unpack . T.strip <$> readFirstLine commonFile
            canonicalizePath $
                if isAbsolute commonPath
                    then commonPath
                    else gitDirectory </> commonPath
        else canonicalizePath gitDirectory

readFirstLine :: FilePath -> IO T.Text
readFirstLine path = do
    content <- T.readFile path
    pure . fromMaybe T.empty . safeHead $ T.lines content

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (value : _) = Just value

toPosix :: FilePath -> FilePath
toPosix = fmap replaceSeparator
  where
    replaceSeparator '\\' = '/'
    replaceSeparator char = char

fromPosix :: FilePath -> FilePath
fromPosix = fmap replaceSeparator
  where
    replaceSeparator '/' = pathSeparator
    replaceSeparator char = char

stripSlashes :: FilePath -> FilePath
stripSlashes = L.dropWhileEnd (== '/') . dropWhile (== '/')

stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash path
    | path /= "/" = L.dropWhileEnd (== '/') path
    | otherwise = path
