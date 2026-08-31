{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Data.VCS.Ignore.Git.Internal.Pattern
-- Description : Parsing and evaluation of Git ignore patterns
-- Copyright   : (c) 2020-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : portable
module Data.VCS.Ignore.Git.Internal.Pattern (
    Pattern,
    PatternGroup (..),
    parsePatterns,
    loadPatternsFile,
    evaluatePatternGroups,
) where

import Control.Exception (IOException, catch, throwIO)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.VCS.Ignore.Types (PathKind (..))
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    pathIsSymbolicLink,
 )
import System.FilePath (pathSeparator)
import qualified System.FilePath.Glob as G
import System.IO.Error (
    isDoesNotExistError,
    tryIOError,
 )

-- | A compiled Git ignore rule. Its representation is deliberately private so
-- callers cannot construct rules that bypass Git's line parsing semantics.
data Pattern = Pattern
    { patternMatchers :: [G.Pattern]
    , patternNegated :: Bool
    , patternDirectoryOnly :: Bool
    }
    deriving (Eq, Show)

-- | Patterns from one ignore source, scoped to a repository-relative prefix.
-- Prefixes use POSIX separators and are conventionally written as @/@ for the
-- repository root or @/directory/@ for a nested ignore file.
data PatternGroup = PatternGroup
    { patternGroupPrefix :: FilePath
    , patternGroupPatterns :: [Pattern]
    }
    deriving (Eq, Show)

-- | Parses the contents of an ignore file. Blank lines and comments are
-- discarded. CRLF, escaped leading hash/bang characters and Git's trailing
-- space rules are handled before compiling each pattern.
parsePatterns :: T.Text -> [Pattern]
parsePatterns = foldr parseLine [] . T.lines
  where
    parseLine raw patterns =
        case prepareLine raw of
            Nothing -> patterns
            Just line -> compilePattern line : patterns

-- | Loads and parses an ignore file. The flag controls whether a symbolic link
-- may be followed: global and repository exclude files use 'True', while a
-- working-tree @.gitignore@ uses 'False' to match Git's behaviour.
--
-- A missing file is an empty source. Other I/O failures are propagated.
loadPatternsFile :: Bool -> FilePath -> IO [Pattern]
loadPatternsFile followSymbolicLink path = do
    symbolicLinkResult <- tryIOError $ pathIsSymbolicLink path
    case symbolicLinkResult of
        Left error'
            | isDoesNotExistError error' -> pure []
            | otherwise -> ioError error'
        Right True
            | not followSymbolicLink -> pure []
            | otherwise -> do
                isRegularFile <- doesFileExist path
                if isRegularFile then parsePatterns <$> readPatterns else pure []
        Right False -> do
            isDirectory <- doesDirectoryExist path
            if isDirectory
                then pure []
                else do
                    isRegularFile <- doesFileExist path
                    if isRegularFile then parsePatterns <$> readPatterns else pure []
  where
    readPatterns = T.readFile path `catch` handleMissing
    handleMissing error'
        | isDoesNotExistError error' = pure T.empty
        | otherwise = throwIO (error' :: IOException)

-- | Evaluates groups ordered from lowest to highest precedence. Within each
-- group, patterns retain file order. Consequently, the last matching rule is
-- authoritative. The result is 'True' when the path is ignored.
evaluatePatternGroups :: PathKind -> [PatternGroup] -> FilePath -> Bool
evaluatePatternGroups kind groups path =
    fromMaybe False $ L.foldl' applyGroup Nothing groups
  where
    candidate = addLeadingSlash . toPosix $ path
    applyGroup result group
        | groupApplies prefix candidate =
            L.foldl' (applyPattern prefix) result $ patternGroupPatterns group
        | otherwise = result
      where
        prefix = normalizePrefix $ patternGroupPrefix group
    applyPattern prefix result pattern'
        | matchesPattern kind pattern' (pathForGroup prefix candidate) =
            Just . not $ patternNegated pattern'
        | otherwise = result

prepareLine :: T.Text -> Maybe T.Text
prepareLine raw
    | T.null line = Nothing
    | "#" `T.isPrefixOf` line = Nothing
    | otherwise = Just line
  where
    line = stripUnescapedTrailingSpaces . T.dropWhileEnd (== '\r') $ raw

compilePattern :: T.Text -> Pattern
compilePattern line =
    Pattern
        { patternMatchers =
            if hasDanglingEscape body
                then []
                else compileGlob . T.unpack <$> matcherSources patternBody directoryOnly
        , patternNegated = negated
        , patternDirectoryOnly = directoryOnly
        }
  where
    (negated, body) = case T.uncons line of
        Just ('!', rest) -> (True, rest)
        _ -> (False, line)
    directoryOnly = "/" `T.isSuffixOf` body
    withoutDirectoryMarker = fromMaybe body $ T.stripSuffix "/" body
    patternBody = unescapePattern withoutDirectoryMarker

matcherSources :: T.Text -> Bool -> [T.Text]
matcherSources raw directoryOnly
    | T.null raw = []
    | directoryOnly = [expandTrailingRecursive scoped]
    | "/**" `T.isSuffixOf` scoped = [expandTrailingRecursive scoped]
    | otherwise = [scoped]
  where
    scoped = case T.stripPrefix "/" raw of
        Just anchored -> "/" <> anchored
        Nothing
            | "**/" `T.isPrefixOf` raw -> raw
            | "/" `T.isInfixOf` raw -> "/" <> raw
            | otherwise -> "**/" <> raw

-- Glob recognizes recursive wildcards in the @**/@ form. Git also gives a
-- trailing @/**@ recursive meaning, so append a final wildcard component to
-- preserve that behaviour at arbitrary depth.
expandTrailingRecursive :: T.Text -> T.Text
expandTrailingRecursive source
    | "/**" `T.isSuffixOf` source = source <> "/*"
    | otherwise = source

compileGlob :: String -> G.Pattern
compileGlob =
    G.compileWith
        G.compDefault
            { G.numberRanges = False
            , G.pathSepInRanges = False
            }

matchesPattern :: PathKind -> Pattern -> FilePath -> Bool
matchesPattern kind pattern' candidate =
    (not (patternDirectoryOnly pattern') || kind == Directory)
        && any (`G.match` candidate) (patternMatchers pattern')

pathForGroup :: FilePath -> FilePath -> FilePath
pathForGroup "/" candidate = candidate
pathForGroup prefix candidate =
    addLeadingSlash . fromMaybe candidate $ L.stripPrefix prefix candidate

groupApplies :: FilePath -> FilePath -> Bool
groupApplies "/" _ = True
groupApplies prefix candidate =
    prefix `L.isPrefixOf` addTrailingSlash candidate
        && prefix /= addTrailingSlash candidate

normalizePrefix :: FilePath -> FilePath
normalizePrefix prefix
    | stripped == "" = "/"
    | otherwise = addTrailingSlash . addLeadingSlash $ stripped
  where
    stripped = dropWhile (== '/') . L.dropWhileEnd (== '/') . toPosix $ prefix

stripUnescapedTrailingSpaces :: T.Text -> T.Text
stripUnescapedTrailingSpaces text = case T.unsnoc text of
    Just (prefix, ' ') ->
        let slashes = T.takeWhileEnd (== '\\') prefix
            beforeSlashes = T.dropEnd (T.length slashes) prefix
         in if odd (T.length slashes)
                then beforeSlashes <> T.dropEnd 1 slashes <> " "
                else stripUnescapedTrailingSpaces prefix
    _ -> text

hasDanglingEscape :: T.Text -> Bool
hasDanglingEscape = odd . T.length . T.takeWhileEnd (== '\\')

unescapePattern :: T.Text -> T.Text
unescapePattern = T.pack . go . T.unpack
  where
    go [] = []
    go ['\\'] = ['\\']
    go ('[' : rest) =
        case takeCharacterClass [] rest of
            Nothing -> "[[]" <> go rest
            Just (body, remaining) ->
                '[' : (unescapeClass body <> (']' : go remaining))
    go ('\\' : char : rest) = escapeGlobLiteral char <> go rest
    go (char : rest) = char : go rest

    takeCharacterClass _ [] = Nothing
    takeCharacterClass prefix ('\\' : char : rest) =
        takeCharacterClass (char : '\\' : prefix) rest
    takeCharacterClass prefix (']' : rest) = Just (reverse prefix, rest)
    takeCharacterClass prefix (char : rest) =
        takeCharacterClass (char : prefix) rest

    unescapeClass body =
        case unescapeClassBody body of
            (False, characters) -> characters
            (True, '!' : characters) -> '!' : '-' : characters
            (True, '^' : characters) -> '^' : '-' : characters
            (True, characters) -> '-' : characters

    unescapeClassBody [] = (False, [])
    unescapeClassBody ('\\' : '-' : rest) =
        let (_, characters) = unescapeClassBody rest
         in (True, characters)
    unescapeClassBody ('\\' : char : rest) =
        let (hasLiteralHyphen, characters) = unescapeClassBody rest
         in (hasLiteralHyphen, escapeGlobLiteral char <> characters)
    unescapeClassBody (char : rest) =
        let (hasLiteralHyphen, characters) = unescapeClassBody rest
         in (hasLiteralHyphen, char : characters)

    escapeGlobLiteral '*' = "[*]"
    escapeGlobLiteral '?' = "[?]"
    escapeGlobLiteral '[' = "[[]"
    escapeGlobLiteral char = [char]

addLeadingSlash :: FilePath -> FilePath
addLeadingSlash path
    | "/" `L.isPrefixOf` path = path
    | otherwise = '/' : path

addTrailingSlash :: FilePath -> FilePath
addTrailingSlash path
    | "/" `L.isSuffixOf` path = path
    | otherwise = path <> "/"

toPosix :: FilePath -> FilePath
toPosix = fmap replaceSeparator
  where
    replaceSeparator char
        | char == pathSeparator = '/'
        | otherwise = char
