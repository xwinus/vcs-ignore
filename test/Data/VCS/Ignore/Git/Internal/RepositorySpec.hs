{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.Git.Internal.RepositorySpec (spec) where

import Control.Concurrent (
    forkIO,
    killThread,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
 )
import Control.Exception (
    IOException,
    SomeException,
    bracket_,
    try,
 )
import Control.Monad (replicateM)
import Data.Either (isLeft, isRight)
import Data.IORef (
    atomicModifyIORef',
    newIORef,
    readIORef,
 )
import Data.VCS.Ignore.Git.Internal.Pattern (
    PatternGroup (..),
 )
import Data.VCS.Ignore.Git.Internal.Repository
import Data.VCS.Ignore.Types (
    GitError (..),
    PathKind (..),
 )
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
 )
import System.Environment (
    lookupEnv,
    setEnv,
    unsetEnv,
 )
import System.FilePath (
    pathSeparator,
    takeDirectory,
    (</>),
 )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
    describe "openGitRepository" $ do
        repositoryExample "opens a repository without scanning nested rules" $ \root -> do
            let nestedIgnore = root </> "nested" </> ".gitignore"
            writeText nestedIgnore "old.txt\n"
            repository <- openGitRepository root
            writeText nestedIgnore "new.txt\n"

            isIgnored repository RegularFile "nested/new.txt" `shouldReturn` True
            isIgnored repository RegularFile "nested/old.txt" `shouldReturn` False

        it "rejects a directory without Git metadata"
            . withSystemTempDirectory "vcs-ignore-invalid"
            $ \root ->
                openGitRepository root `shouldThrow` isNotRepository

        it "resolves gitfile and commondir metadata"
            . withIsolatedEnvironment
            $ \sandbox -> do
                let root = sandbox </> "repo"
                    gitDirectory = sandbox </> "admin" </> "worktrees" </> "main"
                    commonDirectory = sandbox </> "admin" </> "common"
                createDirectoryIfMissing True root
                createDirectoryIfMissing True gitDirectory
                createDirectoryIfMissing True $ commonDirectory </> "info"
                writeText (root </> ".git") "gitdir: ../admin/worktrees/main\n"
                writeText (gitDirectory </> "commondir") "../../common\n"
                writeText (commonDirectory </> "info" </> "exclude") "from-info.txt\n"

                repository <- openGitRepository root
                canonicalGitDirectory <- canonicalizePath gitDirectory
                canonicalCommon <- canonicalizePath commonDirectory
                gitRepositoryGitDirectory repository `shouldBe` canonicalGitDirectory
                gitRepositoryCommonDirectory repository `shouldBe` canonicalCommon
                isIgnored repository RegularFile "from-info.txt" `shouldReturn` True

        it "reports malformed gitfile metadata"
            . withIsolatedEnvironment
            $ \sandbox -> do
                let root = sandbox </> "repo"
                createDirectoryIfMissing True root
                writeText (root </> ".git") "not a gitdir\n"

                openGitRepository root `shouldThrow` isInvalidMetadata

        it "recognizes in-tree gitdir and commondir paths as metadata"
            . withIsolatedEnvironment
            $ \sandbox -> do
                let root = sandbox </> "repo"
                    gitDirectory = root </> ".metadata" </> "worktree"
                    commonDirectory = root </> ".metadata" </> "common"
                createDirectoryIfMissing True gitDirectory
                createDirectoryIfMissing True commonDirectory
                writeText (root </> ".git") "gitdir: .metadata/worktree\n"
                writeText (gitDirectory </> "commondir") "../common\n"

                repository <- openGitRepository root
                isRepositoryMetadata repository ".metadata/worktree" `shouldBe` True
                isRepositoryMetadata repository ".metadata/worktree/index" `shouldBe` True
                isRepositoryMetadata repository ".metadata/common" `shouldBe` True
                isRepositoryMetadata repository ".metadata/working-file" `shouldBe` False

    describe "findGitRepository" $ do
        repositoryExample "finds the nearest enclosing repository" $ \root -> do
            let nested = root </> "one" </> "two"
            createDirectoryIfMissing True nested

            (fmap repositoryRoot <$> findGitRepository nested)
                `shouldReturnJustCanonicalPath` root

        it "returns Nothing when no repository encloses the path"
            . withIsolatedEnvironment
            $ \sandbox ->
                (fmap repositoryRoot <$> findGitRepository sandbox)
                    `shouldReturn` Nothing

    describe "repository rule context" $ do
        repositoryExample "applies global, info, root, and nested rules in order" $ \root -> do
            let sandbox = takeDirectory root
            writeText (sandbox </> "xdg" </> "git" </> "ignore") "*.cache\n"
            writeText (root </> ".git" </> "info" </> "exclude") "*.log\n"
            writeText (root </> ".gitignore") "!keep.log\n"
            writeText (root </> "nested" </> ".gitignore") "!keep.cache\n"
            repository <- openGitRepository root

            isIgnored repository RegularFile "drop.cache" `shouldReturn` True
            isIgnored repository RegularFile "drop.log" `shouldReturn` True
            isIgnored repository RegularFile "keep.log" `shouldReturn` False
            isIgnored repository RegularFile "nested/keep.cache" `shouldReturn` False

        repositoryExample "caches each directory rule group after first use" $ \root -> do
            let ignoreFile = root </> ".gitignore"
            writeText ignoreFile "cached.txt\n"
            repository <- openGitRepository root
            isIgnored repository RegularFile "cached.txt" `shouldReturn` True

            writeText ignoreFile ""
            isIgnored repository RegularFile "cached.txt" `shouldReturn` True

        repositoryExample "shares a cached rule load between concurrent queries" $ \root -> do
            writeText (root </> ".gitignore") "shared.txt\n"
            repository <- openGitRepository root
            start <- newEmptyMVar
            outputs <- replicateM 8 newEmptyMVar

            mapM_
                ( \output -> do
                    _ <- forkIO $ do
                        readMVar start
                        isIgnored repository RegularFile "shared.txt" >>= putMVar output
                    pure ()
                )
                outputs
            putMVar start ()

            results <- mapM readMVar outputs
            results `shouldBe` replicate 8 True

        repositoryExample "retries a shared rule load after owner cancellation" $ \root -> do
            repository <- openGitRepository root
            calls <- newIORef (0 :: Int)
            firstLoadStarted <- newEmptyMVar
            blockFirstLoad <- newEmptyMVar
            ownerResult <- newEmptyMVar
            waiterResult <- newEmptyMVar
            let loader _ = do
                    call <- atomicModifyIORef' calls $ \count -> (count + 1, count + 1)
                    if call == 1
                        then putMVar firstLoadStarted () >> takeMVar blockFirstLoad >> pure []
                        else pure []

            owner <- forkIO $ do
                result <- try @SomeException $ loadDirectoryRulesWith repository loader "."
                putMVar ownerResult result
            takeMVar firstLoadStarted
            _ <- forkIO $ do
                result <- try @SomeException $ loadDirectoryRulesWith repository loader "."
                putMVar waiterResult result
            killThread owner

            firstResult <- takeMVar ownerResult
            secondResult <- takeMVar waiterResult
            firstResult `shouldSatisfy` isLeft
            secondResult `shouldSatisfy` isRight
            readIORef calls `shouldReturn` 2

        repositoryExample "caches synchronous rule-loading failures" $ \root -> do
            repository <- openGitRepository root
            calls <- newIORef (0 :: Int)
            let loader _ = do
                    atomicModifyIORef' calls $ \count -> (count + 1, ())
                    ioError $ userError "rule load failed"

            firstResult <- try @IOException $ loadDirectoryRulesWith repository loader "."
            secondResult <- try @IOException $ loadDirectoryRulesWith repository loader "."
            firstResult `shouldSatisfy` isLeft
            secondResult `shouldSatisfy` isLeft
            readIORef calls `shouldReturn` 1

        repositoryExample "does not load rules below an ignored parent" $ \root -> do
            writeText (root </> ".gitignore") "build/\n"
            writeText (root </> "build" </> ".gitignore") "!keep.txt\n"
            repository <- openGitRepository root

            isIgnored repository Directory "build" `shouldReturn` True
            isIgnored repository RegularFile "build/keep.txt" `shouldReturn` True

        repositoryExample "does not apply a directory's rules to itself" $ \root -> do
            writeText (root </> "nested" </> ".gitignore") "nested/\nself.txt\n"
            repository <- openGitRepository root

            isIgnored repository Directory "nested" `shouldReturn` False
            isIgnored repository RegularFile "nested/self.txt" `shouldReturn` True

        repositoryExample "returns normalized prefixes for cached groups" $ \root -> do
            repository <- openGitRepository root
            rootGroups <- rootRuleContext repository
            nestedGroup <- loadDirectoryRules repository "nested/deeper"

            fmap patternGroupPrefix rootGroups `shouldBe` ["/", "/"]
            patternGroupPrefix nestedGroup `shouldBe` "/nested/deeper/"

    describe "repository paths" $ do
        it "normalizes valid relative paths" $ do
            validateRepositoryPath "" `shouldBe` Right "."
            validateRepositoryPath "./one//two" `shouldBe` Right "one/two"
            groupPrefix "." `shouldBe` "/"
            groupPrefix "one/two" `shouldBe` "/one/two/"

        it "rejects absolute and escaping paths" $ do
            validateRepositoryPath "/outside" `shouldSatisfy` isLeft
            validateRepositoryPath "../outside" `shouldSatisfy` isLeft
            validateRepositoryPath "inside/../outside" `shouldSatisfy` isLeft
            if pathSeparator == '\\'
                then validateRepositoryPath "C:\\outside" `shouldSatisfy` isLeft
                else validateRepositoryPath "C:\\outside" `shouldBe` Right "C:\\outside"

        repositoryExample "always ignores root Git metadata only" $ \root -> do
            repository <- openGitRepository root

            isRepositoryMetadata repository ".git" `shouldBe` True
            isRepositoryMetadata repository ".git/objects/value" `shouldBe` True
            isRepositoryMetadata repository "nested/.git" `shouldBe` False
            isIgnored repository Directory ".git" `shouldReturn` True
            isIgnored repository RegularFile ".git/config" `shouldReturn` True

repositoryExample :: String -> (FilePath -> Expectation) -> Spec
repositoryExample description action =
    it description . withIsolatedEnvironment $ \sandbox -> do
        let root = sandbox </> "repo"
        createDirectoryIfMissing True $ root </> ".git" </> "info"
        action root

withIsolatedEnvironment :: (FilePath -> IO a) -> IO a
withIsolatedEnvironment action =
    withSystemTempDirectory "vcs-ignore-repository" $ \sandbox ->
        withEnvironment "XDG_CONFIG_HOME" (sandbox </> "xdg") $
            action sandbox

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value action = do
    original <- lookupEnv name
    bracket_ (setEnv name value) (restore original) action
  where
    restore Nothing = unsetEnv name
    restore (Just original) = setEnv name original

writeText :: FilePath -> String -> IO ()
writeText path content = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path content

isNotRepository :: GitError -> Bool
isNotRepository (NotGitRepository _) = True
isNotRepository _ = False

isInvalidMetadata :: GitError -> Bool
isInvalidMetadata (InvalidGitMetadata _ _) = True
isInvalidMetadata _ = False

shouldReturnJustCanonicalPath :: IO (Maybe FilePath) -> FilePath -> Expectation
shouldReturnJustCanonicalPath action expectedPath = do
    expected <- canonicalizePath expectedPath
    action `shouldReturn` Just expected
