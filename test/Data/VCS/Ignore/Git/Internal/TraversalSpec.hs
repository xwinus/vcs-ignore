{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.Git.Internal.TraversalSpec (spec) where

import Control.Exception (
    IOException,
    bracket_,
    try,
 )
import Control.Monad (when)
import Data.IORef (
    modifyIORef',
    newIORef,
    readIORef,
 )
import qualified Data.List as L
import Data.VCS.Ignore.Git.Internal.Repository (
    GitRepository,
    openGitRepository,
 )
import Data.VCS.Ignore.Git.Internal.Traversal
import Data.VCS.Ignore.Types
import System.Directory (
    createDirectoryIfMissing,
    createDirectoryLink,
    removeDirectoryLink,
    removeDirectoryRecursive,
    renameDirectory,
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
    describe "foldRepo" $ do
        it "visits entries in depth-first preorder without emitting the root"
            . withRepository
            $ \root repository -> do
                writeText (root </> "directory" </> "child.txt") "child"
                writeText (root </> "file.txt") "file"

                result <- foldRepo repository [] collectEntry

                entries <- completedValue result
                let paths = entryPath <$> reverse entries
                paths `shouldNotContain` [""]
                paths `shouldMatchList` ["directory", "directory" </> "child.txt", "file.txt"]
                reverse entries
                    `shouldMatchList` [ Entry "directory" Directory
                                      , Entry ("directory" </> "child.txt") RegularFile
                                      , Entry "file.txt" RegularFile
                                      ]
                L.elemIndex ("directory" </> "child.txt") paths
                    `shouldBe` ((+ 1) <$> L.elemIndex "directory" paths)

        it "automatically prunes ignored directories and repository metadata"
            . withRepository
            $ \root repository -> do
                writeText (root </> ".gitignore") "ignored/\n"
                writeText (root </> "ignored" </> "hidden.txt") "hidden"
                writeText (root </> "visible.txt") "visible"

                entries <- listRepo repository

                entryPath <$> entries `shouldMatchList` [".gitignore", "visible.txt"]

        it "loads a visible directory's rules before visiting its children"
            . withRepository
            $ \root repository -> do
                writeText (root </> "nested" </> ".gitignore") "*.tmp\n"
                writeText (root </> "nested" </> "drop.tmp") "ignored"
                writeText (root </> "nested" </> "keep.txt") "visible"

                entries <- listRepo repository

                entryPath <$> entries
                    `shouldMatchList` [ "nested"
                                      , "nested" </> ".gitignore"
                                      , "nested" </> "keep.txt"
                                      ]

        it "honours caller pruning without visiting descendants"
            . withRepository
            $ \root repository -> do
                writeText (root </> "pruned" </> "child.txt") "child"
                writeText (root </> "visible" </> "child.txt") "child"

                result <- foldRepo repository [] $ \entries entry ->
                    pure
                        ( entry : entries
                        , if entryPath entry == "pruned" then Prune else Continue
                        )

                entries <- reverse <$> completedValue result
                entryPath <$> entries
                    `shouldMatchList` ["pruned", "visible", "visible" </> "child.txt"]

        it "treats Prune on a file as Continue"
            . withRepository
            $ \root repository -> do
                writeText (root </> "first.txt") "first"
                writeText (root </> "second.txt") "second"

                result <- foldRepo repository [] $ \entries entry ->
                    pure (entryPath entry : entries, Prune)

                paths <- reverse <$> completedValue result
                paths `shouldMatchList` ["first.txt", "second.txt"]

        it "does not follow a directory replaced by a symlink in the callback"
            . withRepository
            $ \root repository ->
                withSystemTempDirectory "vcs-ignore-replacement" $ \outside -> do
                    let directory = root </> "directory"
                        moved = root </> "moved"
                    writeText (directory </> "inside.txt") "inside"
                    writeText (outside </> "outside.txt") "outside"

                    result <- try @IOException $ createDirectoryLink outside (root </> "probe")
                    case result of
                        Left _ -> pendingWith "directory symbolic links are unavailable"
                        Right _ -> do
                            removeDirectoryLink $ root </> "probe"
                            entries <- listWithReplacement repository directory moved outside
                            entryPath <$> entries `shouldBe` ["directory"]

        it "skips a directory removed by the callback"
            . withRepository
            $ \root repository -> do
                let directory = root </> "directory"
                writeText (directory </> "inside.txt") "inside"

                result <- foldRepo repository [] $ \entries entry -> do
                    when (entryPath entry == "directory") $
                        removeDirectoryRecursive directory
                    pure (entry : entries, Continue)

                reverse <$> completedValue result
                    `shouldReturn` [Entry "directory" Directory]

        it "propagates Stop through all remaining traversal levels"
            . withRepository
            $ \root repository -> do
                writeText (root </> "one" </> "child.txt") "child"
                writeText (root </> "two" </> "child.txt") "child"

                result <- foldRepo repository (0 :: Int) $ \count _ ->
                    pure (count + 1, Stop)

                result `shouldBe` WalkStopped 1

        it "forces the initial accumulator even for an empty repository"
            . withRepository
            $ \_ repository ->
                foldRepo repository (error "strict accumulator" :: Int) keepWalking
                    `shouldThrow` errorCall "strict accumulator"

        it "propagates callback failures"
            . withRepository
            $ \root repository -> do
                writeText (root </> "entry.txt") "entry"

                walkRepo repository (const . ioError $ userError "callback failure")
                    `shouldThrow` anyIOException

    describe "repository traversal wrappers" $ do
        it "does not follow directory symbolic links"
            . withRepository
            $ \root repository ->
                withSystemTempDirectory "vcs-ignore-target" $ \target -> do
                    writeText (target </> "outside.txt") "outside"
                    linkResult <- try @IOException $ createDirectoryLink target (root </> "link")
                    case linkResult of
                        Left _ -> pendingWith "directory symbolic links are unavailable"
                        Right _ -> do
                            entries <- listRepo repository
                            entries `shouldBe` [Entry "link" SymbolicLink]

        it "walkRepo reports normal completion"
            . withRepository
            $ \root repository -> do
                writeText (root </> "entry.txt") "entry"
                result <- walkRepo repository (const $ pure Continue)
                result `shouldBe` WalkCompleted ()

        it "forRepo_ performs an effect for every visible entry"
            . withRepository
            $ \root repository -> do
                writeText (root </> "first.txt") "first"
                writeText (root </> "second.txt") "second"
                visited <- newIORef []

                forRepo_ repository $ \entry ->
                    modifyIORef' visited (entryPath entry :)

                paths <- readIORef visited
                paths `shouldMatchList` ["first.txt", "second.txt"]

collectEntry :: [Entry] -> Entry -> IO ([Entry], WalkAction)
collectEntry entries entry = pure (entry : entries, Continue)

keepWalking :: Int -> Entry -> IO (Int, WalkAction)
keepWalking state _ = pure (state, Continue)

completedValue :: WalkResult a -> IO a
completedValue (WalkCompleted value) = pure value
completedValue (WalkStopped _) = do
    expectationFailure "expected completed traversal"
    error "unreachable"

listWithReplacement :: GitRepository -> FilePath -> FilePath -> FilePath -> IO [Entry]
listWithReplacement repository directory moved outside = do
    result <- foldRepo repository [] $ \entries entry -> do
        when (entryPath entry == "directory") $ do
            renameDirectory directory moved
            createDirectoryLink outside directory
        pure (entry : entries, Continue)
    reverse <$> completedValue result

withRepository :: (FilePath -> GitRepository -> IO a) -> IO a
withRepository action =
    withSystemTempDirectory "vcs-ignore-traversal" $ \sandbox ->
        withEnvironment "XDG_CONFIG_HOME" (sandbox </> "xdg") $ do
            let root = sandbox </> "repository"
            createDirectoryIfMissing True $ root </> ".git" </> "info"
            repository <- openGitRepository root
            action root repository

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
