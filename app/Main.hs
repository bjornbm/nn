{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Applicative
import Data.Either (isLeft, isRight, fromRight, rights, lefts)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack, isSuffixOf)
import qualified Data.Text as T (intercalate, length, replicate)
import qualified Data.Text.IO as T (putStrLn, readFile)
import Path ( Path (..), Abs (..), Rel (..), File (..)
            , parseAbsDir, parseRelFile
            , fromRelFile, fromAbsFile, fromAbsDir
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


defaultEditor = const (return "vi")

{- TODO:
-  Consistent terminology (file or note?)
-  Global option to specify NN_DIR.
-  Shouldn't change cwd when usind list --exec!
-  Option to print full file name including path (in quotes or with with escaped spaces?)
-  Integrate with git (if NN_DIR is a repo): add new files and commit changes after edit. Option to push automatically?
-  nn 2010 doesn't find anything. In fact, mdfind will not find anything with 2010 in it!!
-  nn edit based on search term rather than ID?
-  nn cat works strange (not like search); use ID or search term??
+  nn rename command, takes -i
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
-  nn retag command allow to retag by search terms in addition to ID?
-  makeID to make greater ID if already taken?
-  check to identify identical IDs.
-}


main = do
  command <- parseCommand
  dir <- getEnv "NN_HOME"  -- TODO graceful error handling.
  case command of
    List     {} -> list     dir command
    Cat      {} -> cat      dir command
    Tags     {} -> tags     dir command
    Check    {} -> check    dir command
    Import   {} -> importC  dir command
    New      {} -> new      dir command
    Edit     {} -> edit     dir command
    Obsolete {} -> obsolete dir command
    Rename   {} -> rename   dir command
    Retag    {} -> retag    dir command
    _           -> list     dir command



-- List the names of files matching the terms.
list :: Dir -> Command -> IO ()
list dir (None terms) = mapM_ printFilename =<< getNotes dir Nothing terms
list dir (List _ path Nothing tag terms) = getNotes dir tag terms >>=
  mapM_ (if path then putStrLn . notePath dir else printFilename)

-- Apply command specified with --exec to files matching the terms.
list dir (List _ _ (Just exec) tag terms) = do
  notes <- getNotes dir tag terms
  let cmd:args = words exec
  rawSystem cmd (args ++ map (notePath dir) notes) >>= \case
    ExitSuccess -> return ()
    code        -> print code

tags :: Dir -> Command -> IO ()
tags dir (Tags pop) = do
  ts <- countTags <$> getNotes dir Nothing []
  if pop then mapM_ (uncurry (printf "%3d %s\n")) $ sortOn Down ts
         else mapM_ (putStrLn . snd) ts

cat :: Dir -> Command -> IO ()
cat dir (Cat noheaders id) = do
  notes <- getNotes dir Nothing ["name:"++id]  -- TODO not solid. TODO use tag
  contents <- mapM (T.readFile . notePath dir) notes
  if noheaders
     then T.putStrLn $ T.intercalate "\n" contents
     else T.putStrLn $ T.intercalate "\n\n\n" $ zipWith (\n c -> header (pack $ noteFilename n) <> c) notes contents
  where
    header :: Text -> Text
    header s = s <> "\n" <> T.replicate (T.length s) "=" <> "\n"

-- | Edit either one file selected by ID or all files matching search terms.
  -- If neither ID nor search terms are specified edit the last file.
  -- Specifying both ID and serch terms is a user error.
  --
  -- TODO Make this type of file selection default for most actions?
edit :: Dir -> Command -> IO ()
edit dir (Edit (Just id) [])    = editNotes dir =<< getNote dir id
edit dir (Edit Nothing   [])    = editNotes dir =<< pure <$> getLastNote dir
edit dir (Edit Nothing   terms) = editNotes dir =<< getNotes dir Nothing terms
edit dir (Edit (Just _)  (_:_)) = error "Specify either ID or search terms, not both."  -- TODO: graceful.

editNotes :: Dir -> [Note] -> IO ()
editNotes dir notes = do
  exec <- catchIOError (getEnv "EDITOR") defaultEditor
  if null notes
    then return ()
    else do
      let cmd:args = words exec
      rawSystem cmd (args ++ map (notePath dir) notes) >>= \case
        ExitSuccess -> checkinNotes dir notes >>= \case
            ExitSuccess -> mapM_ printFilename notes
            code        -> print code
        code        -> print code

-- | Mark files as obsolete (prepend a '+' to the file name).
--   TODO make sure selection works as desired.
obsolete :: Dir -> Command -> IO ()
obsolete dir (Obsolete dry id) = getNote dir id >>= modifyNotes dry f dir
  where
    f (Note _ i t n e) = Note True i t n e

-- | Rename file.
--   TODO make sure selection works as desired.
rename :: Dir -> Command -> IO ()
rename dir (Rename dry id name) = getNote dir id >>= modifyNotes dry f dir
  where
    f (Note o i t n e) = Note o i t (unwords name) e

-- | Change the tag of a file.
--   TODO make sure selection works as desired.
retag :: Dir -> Command -> IO ()
retag dir (Retag dry id tag) = getNote dir id >>= modifyNotes dry f dir
  where
    f (Note o i _ n e) = Note o i tag n e

-- | Apply function to the given notes, effectively changing their
  -- filenames. If the dry-run flag is set the renaming is shown but
  -- not performed.
modifyNotes :: Run -> (Note -> Note) -> Dir -> [Note] -> IO ()
modifyNotes Dry f dir = mapM_  -- dry-run
  (\note -> printf "%s would be renamed %s\n"
                         (notePath dir note)
                         (notePath dir $ f note))
modifyNotes Full f dir = mapM_  -- Full run
  (\note -> renameRCS dir note (f note)
         >> printFilename (f note))  -- Show the new filename.

-- List files with bad names.
check :: Dir -> Command -> IO ()
check dir (Check True False) = do
  files <- mdlist dir
  mapM_ T.putStrLn $ filter (isLeft . parse noteParser "")
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
importC :: Dir -> Command -> IO ()
importC dir (Import (Just title) tag file) = importC' dir tag title =<< parseRelFile file
importC dir (Import Nothing tag file) = do
  file' <- parseRelFile file
  title <- unpack . filename' <$> file' -<.> ""
  importC' dir tag title file'

importC' :: Dir -> String -> String -> Path Rel File -> IO ()
importC' dir tag title file = do
  id <- makeID
  let note = Note False id tag title (fileExtension file)
  newfile <- noteAbsFile dir note
  copyFile file newfile
  checkinNote dir note >>= \case
    ExitSuccess -> printFilename note
    code        -> print code

new :: Dir -> Command -> IO ()
new dir (New empty tag name) = do
  id <- makeID
  let note = Note False id tag (unwords name) ".txt"
  exec <- if empty then return "touch"  -- TODO: use Haskell actions for file creation instead.
                  else catchIOError (getEnv "EDITOR") defaultEditor
  let cmd:args = words exec
  rawSystem cmd (args ++ [notePath dir note]) >>= \case
    ExitSuccess -> checkinNote dir note >>= \case
      ExitSuccess -> printFilename note
      code        -> print code
    code        -> print code

getNotes :: Dir -> Maybe Tag -> [String] -> IO [Note]
getNotes dir Nothing    []    = processNotes notObsolete <$> mdlist dir
getNotes dir Nothing    terms = processNotes notObsolete <$> mdfind dir terms
getNotes dir (Just tag) terms = processNotes (f tag)     <$> mdfind dir (tag:terms)
  where
    f tag note = notObsolete note && hasTag tag note

processNotes :: (Note -> Bool) -> [Path Abs File] -> [Note]
processNotes f = filter f . rights . map (parse noteParser "" . filename')

getLastNote :: Dir -> IO Note
getLastNote dir = last <$> getNotes dir Nothing []

-- TODO getNote should return a single file like getLast. (What if two files have same ID?
getNote :: Dir -> String -> IO [Note]
getNote dir id = processNotes f <$> mdfind dir ["name:"++id]
  where
    f = hasID (ID $ parts id)
