{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Data.VCS.Ignore.Repo.Git
-- Description : Implementation of 'Repo' for /GIT/
-- Copyright   : (c) 2020-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains implementation of 'Repo' /type class/ for the /GIT/ content
-- versioning system. Most of the public functions is exported only to make them
-- visible for tests, end user of this library really shouldn't need to use them.
module Data.VCS.Ignore.Repo.Git (
    Git (..),
    PathKind (..),
    Pattern (..),
    compilePattern,
    matchesPattern,
    parsePatterns,
    loadPatterns,
    findGitIgnores,
    gitIgnorePatterns,
    repoPatterns,
    globalPatterns,
    scanRepo',
    evaluatePatternGroups,
    isIgnored',
    isGitRepo,
) where

import Control.Exception (
    SomeException,
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
import Data.Maybe (
    fromMaybe,
    maybeToList,
 )
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.VCS.Ignore.FileSystem (
    findPaths,
    toPosixPath,
 )
import Data.VCS.Ignore.Repo (
    Repo (..),
    RepoError (..),
 )
import System.Directory (
    XdgDirectory (XdgConfig),
    canonicalizePath,
    doesDirectoryExist,
    getXdgDirectory,
    makeAbsolute,
 )
import System.FilePath (
    makeRelative,
    takeFileName,
    (</>),
 )
import qualified System.FilePath.Glob as G

-- | Data type representing scanned instance of /GIT/ repository.
data Git = Git
    { gitRepoRoot :: FilePath
    -- ^  absolute path to the repository
    , gitPatterns :: [(FilePath, [Pattern])]
    -- ^ ignored patterns
    }
    deriving (Eq, Show)

-- | Identifies how a candidate path should be matched. Git ignore patterns
-- ending in a slash only match directories, so callers must supply this
-- information without requiring the matcher to inspect or follow the path.
data PathKind
    = FilePathKind
    | DirectoryPathKind
    deriving (Eq, Show)

instance Repo Git where
    repoName = const "Git"
    repoRoot = gitRepoRoot
    scanRepo = scanRepo' globalPatterns repoPatterns gitIgnorePatterns isGitRepo
    isIgnored = isIgnored'

-- | Represents single pattern to be used as a rule for ignoring paths.
data Pattern = Pattern
    { pPatterns :: [G.Pattern]
    -- ^ underlying implementation
    , pRaw :: Text
    -- ^ raw textual representation of the pattern
    , pIsNegated :: Bool
    -- ^ if the pattern is negation (starts with @!@)
    }
    deriving (Eq, Show)

instance IsString Pattern where
    fromString = compilePattern . T.pack

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Compiles pattern.
compilePattern ::
    -- | raw pattern as text
    Text ->
    -- | compiled pattern
    Pattern
compilePattern raw =
    let woPrefix = fromMaybe raw $ T.stripPrefix "!" raw
        patterns = r2 . r1 $ woPrefix
     in Pattern
            { pPatterns = fmap (G.compile . T.unpack) patterns
            , pRaw = raw
            , pIsNegated = raw /= woPrefix
            }
  where
    r1 p
        | "/" `T.isPrefixOf` p = p
        | "**/" `T.isPrefixOf` p = p
        | "/" `T.isInfixOf` T.dropWhileEnd (== '/') p = "/" <> p
        | otherwise = "**/" <> p
    r2 p
        | "/" `T.isSuffixOf` p = [p <> "**"]
        | otherwise = [p, p <> "/**"]

-- | Tests whether given path matches against the pattern.
matchesPattern ::
    -- | pattern to match against
    Pattern ->
    -- | path to check
    FilePath ->
    -- | check result
    Bool
matchesPattern ptn path = any (`G.match` path) (pPatterns ptn)

-- | Parses /Glob/ patterns from given text source. Each line in input text is
-- considered to be single pattern. Lines starting with @#@ (comments) and blank
-- lines are skipped.
--
-- >>> parsePatterns "*.xml\n.DS_Store"
-- [Pattern {pPatterns = [compile "**/*.xml",compile "**/*.xml/*"], pRaw = "*.xml", pIsNegated = False},Pattern {pPatterns = [compile "**/.DS_Store",compile "**/.DS_Store/*"], pRaw = ".DS_Store", pIsNegated = False}]
parsePatterns ::
    -- | text to parse
    Text ->
    -- | parsed patterns
    [Pattern]
parsePatterns = fmap compilePattern . filter (not . excluded) . T.lines
  where
    excluded = \line -> or $ fmap ($ T.stripStart line) [comment, T.null]
    comment = \line -> "#" `T.isPrefixOf` line

-- | Loads /Glob/ patterns from given text file. If the fille cannot be read for
-- any reason, empty list is returned. See 'parsePatterns' for more details
-- about parsing.
loadPatterns ::
    (MonadIO m) =>
    -- | path to text file to parse
    FilePath ->
    -- | parsed /Glob/ patterns
    m [Pattern]
loadPatterns path = parsePatterns <$> liftIO content
  where
    content = catch (T.readFile path) (\(_ :: SomeException) -> pure T.empty)

-- | Recursively finds all @.gitignore@ files within the given directory path.
findGitIgnores ::
    (MonadIO m) =>
    -- | path to the directory to search in
    FilePath ->
    -- | paths of found @.gitignore@ files
    m [FilePath]
findGitIgnores repoDir = findPaths repoDir isGitIgnore
  where
    isGitIgnore path = pure $ takeFileName path == ".gitignore"

-- | Recursively finds all @.gitignore@ files within the given directory path
-- and parses them into /Glob/ patterns. See 'loadPatterns' and 'findGitIgnores'
-- for more details.
gitIgnorePatterns ::
    (MonadIO m) =>
    -- | path to the directory to search @.gitignore@ files in
    FilePath ->
    -- | list of @.gitignore@ paths and parsed /Glob/ patterns
    m [(FilePath, [Pattern])]
gitIgnorePatterns repoDir = do
    gitIgnores <- findGitIgnores repoDir
    mapM (\p -> (toPosixPath . path $ p,) <$> loadPatterns p) gitIgnores
  where
    path p = stripSuffix' ".gitignore" $ stripPrefix' repoDir p

-- | Loads /GIT/ repository specific ignore patterns, present in
-- @REPO_ROOT\/.git\/info\/exclude@ file.
repoPatterns ::
    (MonadIO m) =>
    -- | path to the /GIT/ repository root
    FilePath ->
    -- | parsed /Glob/ patterns
    m [Pattern]
repoPatterns repoDir = loadPatterns $ repoDir </> ".git" </> "info" </> "exclude"

-- | Loads global /GIT/  ignore patterns, present in
-- @XDG_CONFIG_GOME\/git\/ignore@ file.
globalPatterns :: (MonadIO m) => m [Pattern]
globalPatterns =
    (liftIO . getXdgDirectory XdgConfig $ ("git" </> "ignore")) >>= loadPatterns

-- | Internal version of 'scanRepo', where individual functions needs to be
-- explicitly provided, which is useful mainly for testing purposes.
scanRepo' ::
    (MonadIO m, MonadThrow m) =>
    -- | reference to 'globalPatterns' function (or similar)
    m [Pattern] ->
    -- | reference to 'repoPatterns' function (or similar)
    (FilePath -> m [Pattern]) ->
    -- | reference to 'gitIgnorePatterns' function (or similar)
    (FilePath -> m [(FilePath, [Pattern])]) ->
    -- | reference to 'isGitRepo' function (or similar)
    (FilePath -> m Bool) ->
    -- | path to /GIT/ repository root
    FilePath ->
    -- | scanned /Git/ repository
    m Git
scanRepo' globalPatternsFn repoPatternsFn gitIgnoresFn isGitRepoFn repoDir = do
    absRepoDir <- liftIO $ makeAbsolute repoDir
    gitRepo <- isGitRepoFn absRepoDir
    (if gitRepo then proceed else abort) absRepoDir
  where
    abort repoDir' = throwM $ InvalidRepo repoDir' "not a valid GIT repository"
    proceed repoDir' = do
        globalPatterns' <- globalPatternsFn
        repoPatterns' <- repoPatternsFn repoDir'
        gitIgnores <- gitIgnoresFn repoDir'
        let (r, o) = sep gitIgnores
            patterns = [("/", globalPatterns' <> repoPatterns' <> r)] <> o
        pure Git{gitRepoRoot = repoDir', gitPatterns = patterns}
    sep xs =
        let predicate = \(p, _) -> p == "/"
            woRoot = filter (not . predicate) xs
            root = concat . maybeToList $ snd <$> L.find predicate xs
         in (root, woRoot)

-- | Purely evaluates ordered pattern groups for a repository-relative path.
-- Groups and their patterns must be ordered from lowest to highest precedence;
-- the last matching pattern determines the result.
evaluatePatternGroups ::
    PathKind ->
    [(FilePath, [Pattern])] ->
    FilePath ->
    Bool
evaluatePatternGroups kind groups path =
    fromMaybe False $ L.foldl' applyPattern Nothing applicablePatterns
  where
    candidate = addPrefix "/" . toPosix $ path
    applicablePatterns = do
        (prefix, patterns) <- groups
        if groupApplies prefix candidate
            then fmap (prefix,) patterns
            else []
    applyPattern result (prefix, pattern')
        | matchesPattern pattern' (pathForGroup prefix pattern') =
            Just . not $ pIsNegated pattern'
        | otherwise = result
    pathForGroup prefix pattern' =
        addPrefix "/"
            . stripPrefix' (normalizePrefix prefix)
            $ candidateForPattern pattern'
    candidateForPattern pattern'
        | kind == DirectoryPathKind && isDirectoryPattern pattern' =
            addTrailingSlash candidate
        | otherwise = stripTrailingSlash candidate
    isDirectoryPattern = T.isSuffixOf "/" . T.dropWhile (== '!') . pRaw

-- | Internal version of 'isIgnored' function.
isIgnored' ::
    (MonadIO m) =>
    -- | scanned /GIT/ repository
    Git ->
    -- | path to check if ignored
    FilePath ->
    m Bool -- @True@ if given path is ignored
isIgnored' git@(Git _ patterns) path = do
    np <- toPosixPath <$> normalize (repoRoot git) path
    let ignored = any (check2 np False) (filtered np)
        negated = any (check2 np True) (filtered np)
    pure $ ignored && not negated
  where
    sanitized = addPrefix "/"
    asRepoPath = \np -> (`stripPrefix'` sanitized np)
    filtered = \np -> filter (onPath np) patterns
    onPath = \np (p, _) -> p `L.isPrefixOf` sanitized np
    check2 = \np negated (prefix, ptns) ->
        any (`matchesPattern` asRepoPath np prefix)
            . filter (\p -> pIsNegated p == negated)
            $ ptns

-- | Checks whether given directory path is valid /GIT/ repository.
isGitRepo ::
    (MonadIO m) =>
    -- | path to the directory to check
    FilePath ->
    -- | @True@ if the given directory is valid repository
    m Bool
isGitRepo path = liftIO . doesDirectoryExist $ path </> ".git"

------------------------------  PRIVATE FUNCTIONS  -----------------------------

addPrefix :: String -> String -> String
addPrefix prefix str
    | prefix `L.isPrefixOf` str = str
    | otherwise = prefix <> str

stripPrefix' :: String -> String -> String
stripPrefix' prefix str =
    maybe str T.unpack (T.stripPrefix (T.pack prefix) (T.pack str))

stripSuffix' :: String -> String -> String
stripSuffix' suffix str =
    maybe str T.unpack (T.stripSuffix (T.pack suffix) (T.pack str))

toPosix :: FilePath -> FilePath
toPosix = fmap replaceSeparator
  where
    replaceSeparator '\\' = '/'
    replaceSeparator char = char

normalizePrefix :: FilePath -> FilePath
normalizePrefix prefix
    | normalized == "/" = normalized
    | otherwise = addTrailingSlash normalized
  where
    normalized = addPrefix "/" . stripTrailingSlash . toPosix $ prefix

groupApplies :: FilePath -> FilePath -> Bool
groupApplies prefix path =
    normalized == "/"
        || ( normalized `L.isPrefixOf` candidateDirectory
                && normalized /= candidateDirectory
           )
  where
    normalized = normalizePrefix prefix
    candidateDirectory = addTrailingSlash path

addTrailingSlash :: FilePath -> FilePath
addTrailingSlash path
    | "/" `L.isSuffixOf` path = path
    | otherwise = path <> "/"

stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash path
    | path /= "/" = stripSuffix' "/" path
    | otherwise = path

normalize :: (MonadIO m) => FilePath -> FilePath -> m FilePath
normalize repoDir path = do
    canonicalized <- liftIO . canonicalizePath $ repoDir </> stripPrefix' "/" path
    isDir <- liftIO $ doesDirectoryExist canonicalized
    let suffix = if isDir || "/" `L.isSuffixOf` path then "/" else ""
    pure $ makeRelative repoDir canonicalized <> suffix
