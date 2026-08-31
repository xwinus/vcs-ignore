{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.Git.Internal.PatternSpec where

import Control.Exception (
    IOException,
    bracket_,
    try,
 )
import qualified Data.Text as T
import Data.VCS.Ignore.Git.Internal.Pattern
import Data.VCS.Ignore.Types (PathKind (..))
import System.Directory (
    createDirectory,
    createFileLink,
    getPermissions,
    readable,
    setPermissions,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
    describe "evaluatePatternGroups" $ do
        it "uses the last matching pattern across ordered groups" $ do
            let groups =
                    [ group "/" "*.log\n!keep.log\n"
                    , group "/nested/" "keep.log\n!keep.log\n"
                    ]
            evaluatePatternGroups RegularFile groups "drop.log" `shouldBe` True
            evaluatePatternGroups RegularFile groups "keep.log" `shouldBe` False
            evaluatePatternGroups RegularFile groups "nested/keep.log" `shouldBe` False

        it "handles Git line parsing edge cases" $ do
            let groups =
                    [ group
                        "/"
                        "# comment\r\n\\#literal\r\n\\!important\r\ntrailing   \r\nescaped\\ \r\n"
                    ]
            evaluatePatternGroups RegularFile groups "#literal" `shouldBe` True
            evaluatePatternGroups RegularFile groups "!important" `shouldBe` True
            evaluatePatternGroups RegularFile groups "trailing" `shouldBe` True
            evaluatePatternGroups RegularFile groups "trailing   " `shouldBe` False
            evaluatePatternGroups RegularFile groups "escaped " `shouldBe` True

        it "applies directory-only patterns only to real directories" $ do
            let groups = [group "/" "build/\n"]
            evaluatePatternGroups Directory groups "build" `shouldBe` True
            evaluatePatternGroups RegularFile groups "build" `shouldBe` False
            evaluatePatternGroups SymbolicLink groups "build" `shouldBe` False
            evaluatePatternGroups Other groups "build" `shouldBe` False

        it "respects anchoring, nested scopes, and recursive wildcards" $ do
            let groups =
                    [ group "/" "/root.txt\na/**/result.txt\ntree/**\n**/marker\n"
                    , group "/nested/" "/local.txt\n"
                    ]
            evaluatePatternGroups RegularFile groups "root.txt" `shouldBe` True
            evaluatePatternGroups RegularFile groups "deep/root.txt" `shouldBe` False
            evaluatePatternGroups RegularFile groups "a/result.txt" `shouldBe` True
            evaluatePatternGroups RegularFile groups "a/b/c/result.txt" `shouldBe` True
            evaluatePatternGroups RegularFile groups "tree/one/two/file" `shouldBe` True
            evaluatePatternGroups RegularFile groups "marker" `shouldBe` True
            evaluatePatternGroups RegularFile groups "deep/marker" `shouldBe` True
            evaluatePatternGroups RegularFile groups "nested/local.txt" `shouldBe` True
            evaluatePatternGroups RegularFile groups "nested/deep/local.txt" `shouldBe` False

        it "does not extend an ordinary pattern to every descendant" $ do
            let groups = [group "/" "foo/*\n!foo/bar/\n"]
            evaluatePatternGroups Directory groups "foo/bar" `shouldBe` False
            evaluatePatternGroups RegularFile groups "foo/bar/file.txt" `shouldBe` False

    describe "loadPatternsFile" $ do
        it "loads a regular ignore file"
            . withSystemTempDirectory "vcs-ignore-patterns"
            $ \directory -> do
                let path = directory </> ".gitignore"
                writeFile path "*.tmp\n"
                patterns <- loadPatternsFile False path
                evaluatePatternGroups RegularFile [PatternGroup "/" patterns] "file.tmp"
                    `shouldBe` True

        it "treats missing, non-regular, and non-followed symbolic-link sources as empty"
            . withSystemTempDirectory "vcs-ignore-patterns"
            $ \directory -> do
                loadPatternsFile False (directory </> "missing") `shouldReturn` []
                let sourceDirectory = directory </> "directory-source"
                createDirectory sourceDirectory
                loadPatternsFile False sourceDirectory `shouldReturn` []
                let target = directory </> "rules"
                    link = directory </> ".gitignore"
                writeFile target "*.tmp\n"
                createFileLink target link
                loadPatternsFile False link `shouldReturn` []

        it "propagates errors from an existing unreadable source when supported"
            . withSystemTempDirectory "vcs-ignore-patterns"
            $ \directory -> do
                let source = directory </> ".gitignore"
                writeFile source "*.tmp\n"
                permissions <- getPermissions source
                result <-
                    bracket_
                        (setPermissions source permissions{readable = False})
                        (setPermissions source permissions)
                        (try @IOException $ loadPatternsFile True source)
                case result of
                    Left _ -> pure ()
                    Right _ -> pendingWith "unreadable file permissions are not enforced"

group :: FilePath -> String -> PatternGroup
group prefix contents = PatternGroup prefix (parsePatterns $ T.pack contents)
