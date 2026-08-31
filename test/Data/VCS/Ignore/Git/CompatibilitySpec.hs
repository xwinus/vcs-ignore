{-# LANGUAGE OverloadedStrings #-}

module Data.VCS.Ignore.Git.CompatibilitySpec (spec) where

import Control.Exception (bracket_)
import Control.Monad (unless)
import Data.VCS.Ignore (
    GitRepository,
    PathKind (..),
    isIgnored,
    openGitRepository,
 )
import System.Directory (createDirectoryIfMissing)
import System.Environment (
    lookupEnv,
    setEnv,
    unsetEnv,
 )
import System.Exit (ExitCode (..))
import System.FilePath (
    takeDirectory,
    (</>),
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Git compatibility" $ do
    gitExample "matches Git for ordered, anchored, recursive, and escaped rules" $ \root repository -> do
        writeText
            (root </> ".gitignore")
            ( unlines
                [ "*.log"
                , "!keep.log"
                , "keep.log"
                , "/root.txt"
                , "artifacts/**/result.bin"
                , "cache/**"
                , "file?.[ch]"
                , "docs/[a-c].md"
                , "build/"
                , "\\#literal"
                , "\\!important"
                ]
            )

        assertMatchesGit repository root RegularFile "drop.log" "drop.log"
        assertMatchesGit repository root RegularFile "keep.log" "keep.log"
        assertMatchesGit repository root RegularFile "root.txt" "root.txt"
        assertMatchesGit repository root RegularFile "nested/root.txt" "nested/root.txt"
        assertMatchesGit repository root RegularFile "artifacts/a/b/result.bin" "artifacts/a/b/result.bin"
        assertMatchesGit repository root RegularFile "cache/a/b/value" "cache/a/b/value"
        assertMatchesGit repository root RegularFile "file1.c" "file1.c"
        assertMatchesGit repository root RegularFile "file10.c" "file10.c"
        assertMatchesGit repository root RegularFile "docs/b.md" "docs/b.md"
        assertMatchesGit repository root Directory "build" "build/"
        assertMatchesGit repository root RegularFile "#literal" "#literal"
        assertMatchesGit repository root RegularFile "!important" "!important"

    gitExample "keeps descendants ignored when their parent is excluded" $ \root repository -> do
        writeText (root </> ".gitignore") "build/\n"
        writeText (root </> "build" </> ".gitignore") "!keep.txt\n"

        assertMatchesGit repository root Directory "build" "build/"
        assertMatchesGit repository root RegularFile "build/keep.txt" "build/keep.txt"

    gitExample "allows re-inclusion while the parent directory remains visible" $ \root repository -> do
        writeText (root </> ".gitignore") "build/*.txt\n!build/keep.txt\n"

        assertMatchesGit repository root Directory "build" "build/"
        assertMatchesGit repository root RegularFile "build/drop.txt" "build/drop.txt"
        assertMatchesGit repository root RegularFile "build/keep.txt" "build/keep.txt"

    gitExample "does not extend a parent wildcard below a re-included directory" $ \root repository -> do
        writeText (root </> ".gitignore") "foo/*\n!foo/bar/\n"

        assertMatchesGit repository root Directory "foo/bar" "foo/bar/"
        assertMatchesGit repository root RegularFile "foo/bar/file.txt" "foo/bar/file.txt"

    gitExample "treats Glob number ranges as Git literals" $ \root repository -> do
        writeText (root </> ".gitignore") "value<1-3>.txt\n"

        assertMatchesGit repository root RegularFile "value1.txt" "value1.txt"
        assertMatchesGit repository root RegularFile "value<1-3>.txt" "value<1-3>.txt"

    gitExample "preserves an escaped hyphen inside a character class" $ \root repository -> do
        writeText (root </> ".gitignore") "[a\\-c].txt\n"

        assertMatchesGit repository root RegularFile "a.txt" "a.txt"
        assertMatchesGit repository root RegularFile "-.txt" "-.txt"
        assertMatchesGit repository root RegularFile "b.txt" "b.txt"
        assertMatchesGit repository root RegularFile "c.txt" "c.txt"

assertMatchesGit :: GitRepository -> FilePath -> PathKind -> FilePath -> FilePath -> Expectation
assertMatchesGit repository root kind repositoryPath gitPath = do
    actual <- isIgnored repository kind repositoryPath
    expected <- gitIgnores root gitPath
    unless
        (actual == expected)
        ( expectationFailure $
            mconcat
                [ "mismatch for "
                , show repositoryPath
                , " (Git path "
                , show gitPath
                , "): library="
                , show actual
                , ", Git="
                , show expected
                ]
        )

gitIgnores :: FilePath -> FilePath -> IO Bool
gitIgnores root path = do
    (exitCode, _, _) <-
        readProcessWithExitCode
            "git"
            [ "-C"
            , root
            , "check-ignore"
            , "--no-index"
            , "--quiet"
            , "--"
            , path
            ]
            ""
    case exitCode of
        ExitSuccess -> pure True
        ExitFailure 1 -> pure False
        ExitFailure code -> expectationFailure ("git check-ignore failed with " <> show code) >> pure False

gitExample :: String -> (FilePath -> GitRepository -> Expectation) -> Spec
gitExample description action =
    it description . withSystemTempDirectory "vcs-ignore-git-compatibility" $ \sandbox ->
        withEnvironment "XDG_CONFIG_HOME" (sandbox </> "xdg") $ do
            let root = sandbox </> "repository"
            createDirectoryIfMissing True root
            initializeGit root
            repository <- openGitRepository root
            action root repository

initializeGit :: FilePath -> IO ()
initializeGit root = do
    (exitCode, _, errors) <- readProcessWithExitCode "git" ["init", "--quiet", root] ""
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code -> expectationFailure $ "git init failed with " <> show code <> ": " <> errors

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value action = do
    original <- lookupEnv name
    bracket_ (setEnv name value) (restore original) action
  where
    restore Nothing = unsetEnv name
    restore (Just originalValue) = setEnv name originalValue

writeText :: FilePath -> String -> IO ()
writeText path content = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path content
