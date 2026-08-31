{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Data.VCS.Ignore.Git.Internal.Repository
-- Description : Lazy Git repository session and ignore queries
-- Copyright   : (c) 2020-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : portable
module Data.VCS.Ignore.Git.Internal.Repository (
    GitRepository (..),
    openGitRepository,
    findGitRepository,
    repositoryRoot,
    isIgnored,
    rootRuleContext,
    loadDirectoryRules,
    loadDirectoryRulesWith,
    isRepositoryMetadata,
    validateRepositoryPath,
    groupPrefix,
) where

import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
 )
import Control.Exception (
    AsyncException,
    SomeException,
    fromException,
    mask,
    throwIO,
    try,
 )
import Control.Monad (unless)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.VCS.Ignore.Git.Internal.Pattern (
    Pattern,
    PatternGroup (..),
    evaluatePatternGroups,
    loadPatternsFile,
 )
import Data.VCS.Ignore.Types (
    GitError (..),
    PathKind (..),
 )
import System.Directory (
    XdgDirectory (XdgConfig),
    canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    makeAbsolute,
 )
import System.FilePath (
    isAbsolute,
    makeRelative,
    pathSeparator,
    takeDirectory,
    (</>),
 )
import qualified System.FilePath.Posix as Posix

-- | An opened Git working tree. Repository-level rules are captured when the
-- session is opened, while working-tree @.gitignore@ files are loaded and
-- cached on first use.
data GitRepository = GitRepository
    { gitRepositoryRoot :: FilePath
    , gitRepositoryGitDirectory :: FilePath
    , gitRepositoryCommonDirectory :: FilePath
    , gitRepositoryBasePatterns :: [Pattern]
    , gitRepositoryRuleCache :: MVar (Map.Map FilePath RulePromise)
    }

data RuleLoadResult
    = RuleLoadFinished (Either SomeException PatternGroup)
    | RuleLoadCancelled

type RulePromise = MVar RuleLoadResult

-- | Open a Git working tree without scanning its contents.
openGitRepository :: FilePath -> IO GitRepository
openGitRepository path = do
    root <- makeAbsolute path >>= canonicalizePath
    isDirectory <- doesDirectoryExist root
    unless isDirectory (throwIO $ NotGitRepository root)
    gitDirectory <- resolveGitDirectory root
    commonDirectory <- resolveCommonDirectory gitDirectory
    globalIgnore <- getXdgDirectory XdgConfig ("git" </> "ignore")
    globalPatterns <- loadPatternsFile True globalIgnore
    repositoryPatterns <-
        loadPatternsFile True (commonDirectory </> "info" </> "exclude")
    cache <- newMVar Map.empty
    pure
        GitRepository
            { gitRepositoryRoot = root
            , gitRepositoryGitDirectory = gitDirectory
            , gitRepositoryCommonDirectory = commonDirectory
            , gitRepositoryBasePatterns = globalPatterns <> repositoryPatterns
            , gitRepositoryRuleCache = cache
            }

-- | Find and open the nearest enclosing Git working tree.
findGitRepository :: FilePath -> IO (Maybe GitRepository)
findGitRepository path = do
    absolute <- makeAbsolute path
    isDirectory <- doesDirectoryExist absolute
    start <- canonicalizePath $ if isDirectory then absolute else takeDirectory absolute
    go start
  where
    go directory = do
        hasMetadata <- gitMetadataExists directory
        let parent = takeDirectory directory
        if hasMetadata
            then Just <$> openGitRepository directory
            else
                if parent == directory
                    then pure Nothing
                    else go parent

-- | Return the canonical absolute root of the working tree.
repositoryRoot :: GitRepository -> FilePath
repositoryRoot = gitRepositoryRoot

-- | Check a repository-relative path against Git ignore rules. The supplied
-- kind is authoritative; the candidate itself is never inspected.
isIgnored :: GitRepository -> PathKind -> FilePath -> IO Bool
isIgnored repository kind rawPath =
    case validateRepositoryPath rawPath of
        Left err -> throwIO err
        Right relative
            | relative == "." -> pure False
            | isRepositoryMetadata repository relative -> pure True
            | otherwise -> do
                initialGroups <- rootRuleContext repository
                evaluateAncestors initialGroups (ancestorDirectories relative)
          where
            evaluateAncestors groups [] =
                pure $ evaluatePatternGroups kind groups relative
            evaluateAncestors groups (directory : directories)
                | evaluatePatternGroups Directory groups directory = pure True
                | otherwise = do
                    directoryGroup <- loadDirectoryRules repository directory
                    evaluateAncestors (groups <> [directoryGroup]) directories

-- | Rules applicable to entries directly under the repository root. Base
-- rules precede the lazily loaded root @.gitignore@ group.
rootRuleContext :: GitRepository -> IO [PatternGroup]
rootRuleContext repository = do
    rootRules <- loadDirectoryRules repository "."
    pure
        [ PatternGroup
            { patternGroupPrefix = "/"
            , patternGroupPatterns = gitRepositoryBasePatterns repository
            }
        , rootRules
        ]

-- | Load and cache the @.gitignore@ belonging to a repository-relative
-- directory. Symlinked ignore files are deliberately not followed.
loadDirectoryRules :: GitRepository -> FilePath -> IO PatternGroup
loadDirectoryRules repository =
    loadDirectoryRulesWith repository $ \directory ->
        loadPatternsFile False $
            repositoryRoot repository
                </> fromPosix directory
                </> ".gitignore"

-- | Variant with an injectable loader for concurrency tests.
loadDirectoryRulesWith ::
    GitRepository ->
    (FilePath -> IO [Pattern]) ->
    FilePath ->
    IO PatternGroup
loadDirectoryRulesWith repository loadPatterns rawDirectory = do
    directory <-
        case validateRepositoryPath rawDirectory of
            Left err -> throwIO err
            Right validDirectory -> pure validDirectory
    mask $ \restore -> do
        (promise, ownsLoad) <-
            modifyMVar (gitRepositoryRuleCache repository) $ \cache ->
                case Map.lookup directory cache of
                    Just existing -> pure (cache, (existing, False))
                    Nothing -> do
                        created <- newEmptyMVar
                        pure (Map.insert directory created cache, (created, True))
        if ownsLoad
            then do
                result <- try . restore $ loadRules directory
                if either isAsyncException (const False) result
                    then do
                        modifyMVar_
                            (gitRepositoryRuleCache repository)
                            (pure . Map.delete directory)
                        putMVar promise RuleLoadCancelled
                        either throwIO pure result
                    else do
                        putMVar promise $ RuleLoadFinished result
                        either throwIO pure result
            else do
                cached <- restore $ readMVar promise
                case cached of
                    RuleLoadFinished result -> either throwIO pure result
                    RuleLoadCancelled ->
                        loadDirectoryRulesWith repository loadPatterns directory
  where
    loadRules directory = do
        patterns <- loadPatterns directory
        pure
            PatternGroup
                { patternGroupPrefix = groupPrefix directory
                , patternGroupPatterns = patterns
                }

    isAsyncException exception =
        isJust (fromException exception :: Maybe AsyncException)

-- | Whether a repository-relative path names the working tree's own Git
-- metadata. Nested @.git@ names belong to nested working trees and are not
-- classified as this repository's metadata.
isRepositoryMetadata :: GitRepository -> FilePath -> Bool
isRepositoryMetadata repository path =
    any (`containsPath` candidate) metadataPaths
  where
    candidate = Posix.normalise $ toPosix path
    metadataPaths =
        ".git"
            : foldr
                addInternalMetadata
                []
                [ gitRepositoryGitDirectory repository
                , gitRepositoryCommonDirectory repository
                ]
    addInternalMetadata absolutePath paths =
        let relative = toPosix $ makeRelative (repositoryRoot repository) absolutePath
         in if isOutside relative || relative == "."
                then paths
                else Posix.normalise relative : paths
    containsPath metadata candidatePath =
        candidatePath == metadata
            || (metadata <> "/") `List.isPrefixOf` candidatePath

-- | Validate and normalize a repository-relative lexical path.
validateRepositoryPath :: FilePath -> Either GitError FilePath
validateRepositoryPath rawPath
    | invalid = Left $ InvalidRepositoryPath rawPath
    | otherwise = Right normalized
  where
    posixPath = toPosix rawPath
    components = Posix.splitDirectories posixPath
    invalid =
        isAbsolute rawPath
            || Posix.isAbsolute posixPath
            || (pathSeparator == '\\' && hasWindowsDrive posixPath)
            || '\NUL' `elem` rawPath
            || ".." `elem` components
    normalized =
        case Posix.normalise posixPath of
            "" -> "."
            value -> value

-- | Convert a repository-relative directory to a normalized pattern-group
-- prefix.
groupPrefix :: FilePath -> FilePath
groupPrefix directory
    | normalized `elem` ["", ".", "/"] = "/"
    | otherwise = "/" <> stripSlashes normalized <> "/"
  where
    normalized = Posix.normalise $ toPosix directory

resolveGitDirectory :: FilePath -> IO FilePath
resolveGitDirectory root = do
    let metadata = root </> ".git"
    isDirectory <- doesDirectoryExist metadata
    if isDirectory
        then canonicalizePath metadata
        else do
            isFile <- doesFileExist metadata
            if isFile
                then resolveGitFile root metadata
                else throwIO $ NotGitRepository root

resolveGitFile :: FilePath -> FilePath -> IO FilePath
resolveGitFile root metadata = do
    firstLine <- readFirstLine metadata
    case Text.stripPrefix "gitdir:" firstLine of
        Nothing ->
            throwIO $
                InvalidGitMetadata metadata "expected a 'gitdir:' declaration"
        Just rawGitDirectory -> do
            let declared = Text.unpack $ Text.strip rawGitDirectory
            if null declared
                then throwIO $ InvalidGitMetadata metadata "empty gitdir path"
                else do
                    let resolved =
                            if isAbsolute declared
                                then declared
                                else root </> declared
                    exists <- doesDirectoryExist resolved
                    unless exists $
                        throwIO (InvalidGitMetadata metadata "gitdir does not exist")
                    canonicalizePath resolved

resolveCommonDirectory :: FilePath -> IO FilePath
resolveCommonDirectory gitDirectory = do
    let commonFile = gitDirectory </> "commondir"
    exists <- doesFileExist commonFile
    if not exists
        then pure gitDirectory
        else do
            rawCommonDirectory <- Text.unpack . Text.strip <$> readFirstLine commonFile
            if null rawCommonDirectory
                then throwIO $ InvalidGitMetadata commonFile "empty commondir path"
                else do
                    let resolved =
                            if isAbsolute rawCommonDirectory
                                then rawCommonDirectory
                                else gitDirectory </> rawCommonDirectory
                    isDirectory <- doesDirectoryExist resolved
                    unless isDirectory $
                        throwIO (InvalidGitMetadata commonFile "commondir does not exist")
                    canonicalizePath resolved

readFirstLine :: FilePath -> IO Text.Text
readFirstLine path = do
    content <- Text.readFile path
    pure . fromMaybe Text.empty . safeHead $ Text.lines content

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (value : _) = Just value

gitMetadataExists :: FilePath -> IO Bool
gitMetadataExists root = do
    let metadata = root </> ".git"
    isDirectory <- doesDirectoryExist metadata
    isFile <- doesFileExist metadata
    pure $ isDirectory || isFile

ancestorDirectories :: FilePath -> [FilePath]
ancestorDirectories relative =
    case directoryComponents of
        [] -> []
        first : rest -> scanl (Posix.</>) first rest
  where
    directory = Posix.takeDirectory relative
    directoryComponents =
        filter (`notElem` ["", ".", "/"]) $
            Posix.splitDirectories directory

hasWindowsDrive :: FilePath -> Bool
hasWindowsDrive (letter : ':' : _) = Char.isAlpha letter
hasWindowsDrive _ = False

isOutside :: FilePath -> Bool
isOutside path =
    Posix.isAbsolute path
        || case Posix.splitDirectories path of
            ".." : _ -> True
            _ -> False

toPosix :: FilePath -> FilePath
toPosix = fmap replaceSeparator
  where
    replaceSeparator character
        | character == pathSeparator = '/'
        | otherwise = character

fromPosix :: FilePath -> FilePath
fromPosix "." = ""
fromPosix path = fmap replaceSeparator path
  where
    replaceSeparator '/' = pathSeparator
    replaceSeparator character = character

stripSlashes :: FilePath -> FilePath
stripSlashes = List.dropWhileEnd (== '/') . dropWhile (== '/')
