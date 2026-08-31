module Main (main) where

import Control.Monad (forM_)
import Criterion.Main (
    bench,
    bgroup,
    defaultMain,
    envWithCleanup,
    nfIO,
    whnfIO,
 )
import Data.VCS.Ignore (
    PathKind (Directory),
    WalkAction (Continue, Stop),
    entryPath,
    foldRepo,
    isIgnored,
    listRepo,
    openGitRepository,
 )
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removePathForcibly,
 )
import System.FilePath (
    takeDirectory,
    (</>),
 )
import System.IO.Temp (createTempDirectory)

main :: IO ()
main =
    defaultMain
        [ envWithCleanup setupEnvironment cleanupEnvironment $ \root ->
            bgroup
                "repository"
                [ bench "open" $ whnfIO (openGitRepository root)
                , bench "cold-directory-query" . whnfIO $ do
                    repo <- openGitRepository root
                    isIgnored repo Directory "ignored"
                , bench "fold-visible-tree" . whnfIO $ do
                    repo <- openGitRepository root
                    foldRepo repo (0 :: Int) countEntry
                , bench "list-visible-tree" . nfIO $ do
                    repo <- openGitRepository root
                    fmap entryPath <$> listRepo repo
                , bench "stop-after-ten" . whnfIO $ do
                    repo <- openGitRepository root
                    foldRepo repo (0 :: Int) stopAfterTen
                ]
        ]
  where
    countEntry count _ = pure (count + 1, Continue)
    stopAfterTen count _
        | count >= 10 = pure (count, Stop)
        | otherwise = pure (count + 1, Continue)

setupEnvironment :: IO FilePath
setupEnvironment = do
    temporary <- getTemporaryDirectory
    sandbox <- createTempDirectory temporary "vcs-ignore-benchmark"
    let root = sandbox </> "repo"
    createDirectoryIfMissing True $ root </> ".git" </> "info"
    writeFile (root </> ".gitignore") "ignored/\n"
    createTree $ root </> "ignored"
    createTree $ root </> "visible"
    pure root
  where
    createTree base =
        forM_ [1 .. directoryCount] $ \directoryIndex -> do
            let directory = base </> show directoryIndex
            createDirectoryIfMissing True directory
            forM_ [1 .. filesPerDirectory] $ \fileIndex ->
                writeFile (directory </> show fileIndex <> ".generated") "benchmark"
    directoryCount = 20 :: Int
    filesPerDirectory = 200 :: Int

cleanupEnvironment :: FilePath -> IO ()
cleanupEnvironment = removePathForcibly . takeDirectory
