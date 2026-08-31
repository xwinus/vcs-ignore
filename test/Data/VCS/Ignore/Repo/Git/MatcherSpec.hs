{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.Repo.Git.MatcherSpec where

import Control.Exception (
    IOException,
    bracket_,
    try,
 )
import qualified Data.ByteString as BS
import Data.VCS.Ignore.Repo (RepoError (..))
import Data.VCS.Ignore.Repo.Git.Matcher
import System.Directory (
    createDirectoryIfMissing,
    createFileLink,
 )
import System.Environment (
    lookupEnv,
    setEnv,
    unsetEnv,
 )
import System.FilePath (
    takeDirectory,
    (</>),
 )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
    describe "findRepoRoot" $ do
        repoExample "finds an enclosing repository without scanning its contents" $ \root -> do
            let nested = root </> "a" </> "b"
            createDirectoryIfMissing True nested
            findRepoRoot nested `shouldReturn` Just root

        it "returns Nothing outside a repository"
            . withSystemTempDirectory "vcs-ignore-outside"
            $ \path ->
                findRepoRoot path `shouldReturn` Nothing

        it "rejects an invalid repository when opening a matcher"
            . withSystemTempDirectory "vcs-ignore-invalid"
            $ \path ->
                openGitIgnoreMatcher path `shouldThrow` isInvalidRepo

    describe "isIgnoredPath" $ do
        repoExample "does not load nested .gitignore files when opening the matcher" $ \root -> do
            let ignore = root </> "deep" </> ".gitignore"
            writeText ignore "old-rule.txt\n"
            matcher <- openGitIgnoreMatcher root

            writeText ignore "new-rule.txt\n"
            isIgnoredPath matcher FilePathKind "deep/new-rule.txt" `shouldReturn` True
            isIgnoredPath matcher FilePathKind "deep/old-rule.txt" `shouldReturn` False

        repoExample "loads rules only along the queried ancestor chain" $ \root -> do
            writeText (root </> "left" </> ".gitignore") "left.txt\n"
            let rightIgnore = root </> "right" </> ".gitignore"
            writeText rightIgnore "old-right.txt\n"
            matcher <- openGitIgnoreMatcher root

            isIgnoredPath matcher FilePathKind "left/left.txt" `shouldReturn` True
            writeText rightIgnore "new-right.txt\n"
            isIgnoredPath matcher FilePathKind "right/new-right.txt" `shouldReturn` True
            isIgnoredPath matcher FilePathKind "right/old-right.txt" `shouldReturn` False

        repoExample "applies root, repository, and nested rules in Git precedence order" $ \root -> do
            writeText (root </> ".git" </> "info" </> "exclude") "*.cache\n"
            writeText (root </> ".gitignore") "!root.cache\n*.log\n"
            writeText (root </> "nested" </> ".gitignore") "!keep.log\nroot.cache\n"
            matcher <- openGitIgnoreMatcher root

            isIgnoredPath matcher FilePathKind "nested/drop.log" `shouldReturn` True
            isIgnoredPath matcher FilePathKind "nested/keep.log" `shouldReturn` False
            isIgnoredPath matcher FilePathKind "nested/root.cache" `shouldReturn` True
            isIgnoredPath matcher FilePathKind "root.cache" `shouldReturn` False

        repoExample "uses the last matching rule within a pattern group" $ \root -> do
            writeText (root </> ".gitignore") "*.txt\n!important.txt\nimportant.txt\n"
            matcher <- openGitIgnoreMatcher root

            isIgnoredPath matcher FilePathKind "important.txt" `shouldReturn` True

        repoExample "does not apply a directory's own rules to the directory itself" $ \root -> do
            writeText (root </> "a" </> ".gitignore") "a/\nself\n"
            matcher <- openGitIgnoreMatcher root

            isIgnoredPath matcher DirectoryPathKind "a" `shouldReturn` False
            isIgnoredPath matcher FilePathKind "a/self" `shouldReturn` True
            isIgnoredPath matcher FilePathKind "ab/self" `shouldReturn` False

        repoExample "stops below an ignored parent so nested negation cannot re-include a child" $ \root -> do
            writeText (root </> ".gitignore") "build/\n"
            writeText (root </> "build" </> ".gitignore") "!keep.txt\n"
            matcher <- openGitIgnoreMatcher root

            isIgnoredPath matcher DirectoryPathKind "build" `shouldReturn` True
            isIgnoredPath matcher FilePathKind "build/keep.txt" `shouldReturn` True

        repoExample "allows re-inclusion when the parent directory remains visible" $ \root -> do
            writeText (root </> ".gitignore") "build/*\n!build/keep.txt\n"
            matcher <- openGitIgnoreMatcher root

            isIgnoredPath matcher DirectoryPathKind "build" `shouldReturn` False
            isIgnoredPath matcher FilePathKind "build/drop.txt" `shouldReturn` True
            isIgnoredPath matcher FilePathKind "build/keep.txt" `shouldReturn` False

        repoExample "distinguishes directory-only rules without inspecting the leaf" $ \root -> do
            writeText (root </> ".gitignore") "only-dir/\ncontents/**\n"
            matcher <- openGitIgnoreMatcher root

            isIgnoredPath matcher DirectoryPathKind "only-dir" `shouldReturn` True
            isIgnoredPath matcher FilePathKind "only-dir" `shouldReturn` False
            isIgnoredPath matcher DirectoryPathKind "contents" `shouldReturn` False
            isIgnoredPath matcher FilePathKind "contents/item" `shouldReturn` True

        repoExample "caches each .gitignore after its first query" $ \root -> do
            let ignore = root </> ".gitignore"
            writeText ignore "cached-once.txt\n"
            matcher <- openGitIgnoreMatcher root
            isIgnoredPath matcher FilePathKind "cached-once.txt" `shouldReturn` True

            writeText ignore ""
            isIgnoredPath matcher FilePathKind "cached-once.txt" `shouldReturn` True

        repoExample "matches a file symlink by its repository name" $ \root ->
            withSystemTempDirectory "vcs-ignore-target" $ \outside -> do
                let target = outside </> "different-name"
                    link = root </> "ignored-link"
                writeText target "target"
                linkResult <- try @IOException $ createFileLink target link
                case linkResult of
                    Left _ -> pendingWith "file symlinks are unavailable on this platform"
                    Right _ -> do
                        writeText (root </> ".gitignore") "ignored-link\n"
                        matcher <- openGitIgnoreMatcher root
                        isIgnoredPath matcher FilePathKind link `shouldReturn` True

        repoExample "does not treat a symlink as a directory for a directory-only rule" $ \root ->
            withSystemTempDirectory "vcs-ignore-target" $ \outside -> do
                let link = root </> "linked-dir"
                linkResult <- try @IOException $ createFileLink outside link
                case linkResult of
                    Left _ -> pendingWith "symlinks are unavailable on this platform"
                    Right _ -> do
                        writeText (root </> ".gitignore") "linked-dir/\n"
                        matcher <- openGitIgnoreMatcher root
                        isIgnoredPath matcher FilePathKind link `shouldReturn` False

        repoExample "rejects relative and absolute paths outside the repository" $ \root -> do
            matcher <- openGitIgnoreMatcher root
            isIgnoredPath matcher FilePathKind "../outside" `shouldThrow` isOutsideRepo
            isIgnoredPath matcher FilePathKind (root <> "-sibling/file")
                `shouldThrow` isOutsideRepo

        repoExample "treats missing and unreadable ignore files as empty" $ \root -> do
            matcherWithMissing <- openGitIgnoreMatcher root
            isIgnoredPath matcherWithMissing FilePathKind "missing.txt" `shouldReturn` False

            BS.writeFile (root </> ".gitignore") $ BS.pack [0xFF, 0xFE]
            matcherWithUnreadable <- openGitIgnoreMatcher root
            isIgnoredPath matcherWithUnreadable FilePathKind "unreadable.txt" `shouldReturn` False

        repoExample "does not follow a symlinked .gitignore" $ \root ->
            withSystemTempDirectory "vcs-ignore-rules" $ \outside -> do
                let rules = outside </> "rules"
                writeText rules "from-symlink.txt\n"
                linkResult <- try @IOException $ createFileLink rules (root </> ".gitignore")
                case linkResult of
                    Left _ -> pendingWith "file symlinks are unavailable on this platform"
                    Right _ -> do
                        matcher <- openGitIgnoreMatcher root
                        isIgnoredPath matcher FilePathKind "from-symlink.txt" `shouldReturn` False

        repoExample "always excludes Git metadata" $ \root -> do
            matcher <- openGitIgnoreMatcher root
            isIgnoredPath matcher DirectoryPathKind ".git" `shouldReturn` True
            isIgnoredPath matcher FilePathKind ".git/objects/object" `shouldReturn` True

repoExample :: String -> (FilePath -> Expectation) -> Spec
repoExample description testAction = it description (withRepository testAction)

isInvalidRepo :: RepoError -> Bool
isInvalidRepo (InvalidRepo _ _) = True

isOutsideRepo :: GitIgnoreMatcherError -> Bool
isOutsideRepo (PathOutsideRepository _ _) = True

withRepository :: (FilePath -> IO a) -> IO a
withRepository action =
    withSystemTempDirectory "vcs-ignore-repo" $ \sandbox ->
        withEnvironment "XDG_CONFIG_HOME" (sandbox </> "xdg") $ do
            let root = sandbox </> "repo"
            createDirectoryIfMissing True $ root </> ".git" </> "info"
            action root

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value action = do
    original <- lookupEnv name
    bracket_ (setEnv name value) (restore original) action
  where
    restore Nothing = unsetEnv name
    restore (Just oldValue) = setEnv name oldValue

writeText :: FilePath -> String -> IO ()
writeText path content = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path content
