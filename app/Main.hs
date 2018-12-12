{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Either (isLeft, isRight)
import Data.List (sort)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack, isSuffixOf)
import qualified Data.Text as T (intercalate, length, replicate)
import qualified Data.Text.IO as T (putStrLn, readFile)
import Path ( Path (..), Abs (..), Rel (..), Dir (..), File (..)
            , parseAbsDir, parseRelFile
            , fromRelFile, fromAbsFile
            , parent, fileExtension
            , (</>), (<.>), (-<.>))
import Path.IO (copyFile)
import System.Environment (getEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (catchIOError)
import System.Process (rawSystem)
import Text.Megaparsec (parse)
import Text.Printf (printf)

import NNUtil
import Options


defaultEditor = const (return "vim")

{- TODO:
-  Consistent terminology (file or note?)
-  Global option to specify NN_DIR.
-  Shouldn't change cwd when usind list --exec!
-  Option to print full file name including path (in quotes or with with escaped spaces?)
-  Integrate with git (if NN_DIR is a repo): add new files and commit changes after edit. Option to push automatically?
-  nn 2010 doesn't find anything. In fact, mdfind will not find anything with 2010 in it!!
-  nn edit based on search term rather than ID?
-  nn cat works strange (not like search); use ID or search term??
-  nn rename command, takes -i
-  nn redate command, takes -i and optional new date (otherwise now)
-  nn pandoc command?
-  nn pretty command? (| pandoc --smart --to=plain)
+  nn obsolete command, takes -i and adds + in front of ID
+  nn list --path switch, returns full path of matching files
-  Consistently use search term and or -i for edit, cat, etc.
-  Allow multiple tags separated by underscore {ID}-tag1_tag2-{TITLE}
-  nn -t tag option for searching by tag
-  nn tag command to add tag to notes
-  nn rmtag command to remove tag from notes
-  nn touch command to update ID to today's time (use `git mv` if repo!)
-  Change ID format from 2014_05_20_1738 to 20140520T1738?
-  nn stats command to list total number of current and obsolete files, etc?
-  nn check files without tags and without titles
-  When listing, indicate empty files (or number of lines in file?)
-  Allow multiple --tag (should they be OR or AND (only meaningful with support for multiple tags))
-}


main = do
  command <- parseCommand
  dir <- parseAbsDir =<< getEnv "NN_HOME"  -- TODO graceful error handling.
  case command of
    List     {} -> list     dir command
    Cat      {} -> cat      dir command
    Tags     {} -> tags     dir command
    Check    {} -> check    dir command
    Import   {} -> importC  dir command
    New      {} -> new      dir command
    Edit     {} -> edit     dir command
    Obsolete {} -> obsolete dir command
    _           -> list     dir command


-- List the names of files matching the terms.
list :: Path Abs Dir -> Command -> IO ()
list dir (None terms) = mapM_ printFilename =<< getFiles dir Nothing terms
list dir (List _ path Nothing tag terms) = getFiles dir tag terms >>=
  mapM_ (if path then putStrLn . fromAbsFile else printFilename)


-- Apply command specified with --exec to files matching the terms.
list dir (List _ _ (Just exec) tag terms) = do
  files <- getFiles dir tag terms
  let cmd:args = words exec
  rawSystem cmd (args ++ map fromAbsFile files) >>= \case
    ExitSuccess -> return ()
    code        -> print code

tags :: Path Abs Dir -> Command -> IO ()
tags dir (Tags pop) = do
  ts <- countTags <$> getFiles dir Nothing []
  if pop then mapM_ (uncurry (printf "%3d %s\n")) $ reverse $ sort ts
         else mapM_ (putStrLn . snd) ts

cat :: Path Abs Dir -> Command -> IO ()
cat dir (Cat noheaders id) = do
  files <- getFiles dir Nothing ["name:"++id]  -- TODO not solid. TODO use tag
  contents <- mapM (T.readFile . fromAbsFile) files
  if noheaders
     then T.putStrLn $ T.intercalate "\n" contents
     else T.putStrLn $ T.intercalate "\n\n\n" $ zipWith (\f c -> header (filename' f) <> c) files contents
  where
    header :: Text -> Text
    header s = s <> "\n" <> T.replicate (T.length s) "=" <> "\n"

edit :: Path Abs Dir -> Command -> IO ()
edit dir (Edit editID) = do
  files <- case editID of
             Just id -> getID dir id
             Nothing -> pure <$> getLast dir
  exec <- catchIOError (getEnv "EDITOR") defaultEditor
  if null files
    then return ()
    else do
      let cmd:args = words exec
      rawSystem cmd (args ++ map fromAbsFile files) >>= \case
        ExitSuccess -> checkin files >>= \case
            ExitSuccess -> mapM_ printFilename files
            code        -> print code
        code        -> print code

-- | Mark files as obsolete (prepend a '+' to the file name).
--   TODO make sure selection works as desired.
obsolete :: Path Abs Dir -> Command -> IO ()
obsolete dir (Obsolete dry id) = do
  files <- getID dir id
  mapM_ (if dry then dryrun else run) files
  where
      obsfile :: Path Abs File -> IO (Path Abs File)
      obsfile file = (parent file </>) <$> parseRelFile ('+' : unpack (filename' file))

      dryrun file = printf "%s would be renamed %s\n" (fromAbsFile file)
                  . fromAbsFile =<< obsfile file

      run file = do
        obs <- obsfile file
        renameRCS file obs
        printFilename obs  -- Show the new filename.

-- List files with bad names.
check :: Path Abs Dir -> Command -> IO ()
check dir (Check True False) = do
  files <- mdlist dir
  mapM_ T.putStrLn $ filter (isLeft . parse filePatternFull "")
                 $ map filename' files

-- List files with bad references.
check dir (Check False True) = putStrLn "NOT IMPLEMENTED" -- TODO

-- List bad files with headers.
check dir (Check False False) = do
  putStrLn "Badly named files"
  putStrLn "-----------------"
  check dir (Check True False)
  putStrLn ""
  putStrLn "Files with duplicate identifiers"
  putStrLn "--------------------------------"
  putStrLn "NOT IMPLEMENTED" -- TODO
  putStrLn ""
  putStrLn "Files with bad references"
  putStrLn "-------------------------"
  check dir (Check False True)

check dir (Check True True) = do
  check dir (Check True False)
  check dir (Check False True)


-- | Import a pre-existing file, optionally with a new title.
importC :: Path Abs Dir -> Command -> IO ()
importC dir (Import (Just title) tag file) = importC' dir tag title =<< parseRelFile file
importC dir (Import Nothing tag file) = do
  file' <- parseRelFile file
  title <- unpack . filename' <$> file' -<.> ""
  importC' dir tag title file'

importC' :: Path Abs Dir -> String -> String -> Path Rel File -> IO ()
importC' dir tag title file = do
  id <- makeID
  newfile <- parseRelFile $ id ++ "-" ++ tag ++ "-" ++ title ++ fileExtension file
  copyFile file (dir </> newfile)
  checkin [dir </> newfile] >>= \case
    ExitSuccess -> putStrLn $ fromRelFile newfile
    code        -> print code

new :: Path Abs Dir -> Command -> IO ()
new dir (New empty tag name) = do
  id <- makeID
  newfile <- parseRelFile $ id ++ "-" ++ tag ++ "-" ++ unwords name ++ ".txt"
  cmd <- if empty then return "touch"  -- TODO: use Haskell actions for file creation instead.
                  else catchIOError (getEnv "EDITOR") defaultEditor
  rawSystem cmd [fromAbsFile (dir </> newfile)] >>= \case
    ExitSuccess -> checkin [dir </> newfile] >>= \case
      ExitSuccess -> putStrLn $ fromRelFile newfile
      code        -> print code
    code        -> print code

getFiles :: Path Abs Dir -> Maybe String -> [String] -> IO [Path Abs File]
getFiles dir Nothing    []    = processFiles  filePattern              <$> mdlist dir
getFiles dir Nothing    terms = processFiles  filePattern              <$> mdfind dir terms
getFiles dir (Just tag) terms = processFiles (filePatternT $ pack tag) <$> mdfind dir (tag:terms)

processFiles :: Parser String -> [Path Abs File] -> [Path Abs File]
processFiles pattern = filter (isRight . parse pattern "" . filename')

getLast :: Path Abs Dir -> IO (Path Abs File)
getLast dir = last <$> getFiles dir Nothing []

getID :: Path Abs Dir -> String -> IO [Path Abs File]
getID dir id =  processFiles (filePatternID $ pack id) <$> mdfind dir ["name:"++id]

