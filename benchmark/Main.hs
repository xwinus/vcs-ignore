{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM_)
import Criterion.Main (
    bench,
    bgroup,
    defaultMain,
    envWithCleanup,
    whnfIO,
 )
import Data.VCS.Ignore (
    Git,
    PathKind (DirectoryPathKind),
    Repo (scanRepo),
    isIgnoredPath,
    openGitIgnoreMatcher,
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
                "ignored-large-tree"
                [ bench "eager-scan" $ whnfIO (scanRepo @Git root)
                , bench
                    "lazy-open-and-prune"
                    ( whnfIO $ do
                        matcher <- openGitIgnoreMatcher root
                        isIgnoredPath matcher DirectoryPathKind "ignored"
                    )
                ]
        ]

setupEnvironment :: IO FilePath
setupEnvironment = do
    temporary <- getTemporaryDirectory
    sandbox <- createTempDirectory temporary "vcs-ignore-benchmark"
    let root = sandbox </> "repo"
    createDirectoryIfMissing True $ root </> ".git" </> "info"
    writeFile (root </> ".gitignore") "ignored/\n"
    forM_ [1 .. directoryCount] $ \directoryIndex -> do
        let directory = root </> "ignored" </> show directoryIndex
        createDirectoryIfMissing True directory
        writeFile (directory </> ".gitignore") "*.generated\n"
        forM_ [1 .. filesPerDirectory] $ \fileIndex ->
            writeFile (directory </> show fileIndex <> ".generated") "benchmark"
    pure root
  where
    directoryCount = 20 :: Int
    filesPerDirectory = 200 :: Int

cleanupEnvironment :: FilePath -> IO ()
cleanupEnvironment = removePathForcibly . takeDirectory
