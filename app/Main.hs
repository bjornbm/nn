{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List (sortOn, groupBy)
import Data.Maybe (maybeToList)
import Data.Ord (Down (Down))
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T (intercalate, length, replicate)
import qualified Data.Text.IO as T (putStrLn, readFile)
import Path ( Path, Rel, File, parseRelFile, fileExtension, (-<.>))
import Path.IO (copyFile)
import System.Environment (getEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (catchIOError)
import System.Process (rawSystem)
import Text.Printf (printf)

import ID
import Util
import Options
import Select


defaultEditor :: b -> IO String
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
+  makeID to make greater ID if already taken?
+  check to identify identical IDs.
+  List in format "ID [tag] title" (without extension)
-}

main :: IO ()
main = do
  command <- parseCommand
  dir <- getEnv "NN_HOME"  -- TODO graceful error handling.
  -- TODO This is really just a pattern match which could be replaced
  -- with a single command. The current implementation just have lots
  -- of partial functions!
  case command of
    List     {} -> execute  dir command
    Cat      {} -> execute  dir command
    Tags     {} -> execute  dir command
    Check    {} -> execute  dir command
    Import   {} -> execute  dir command
    New      {} -> execute  dir command
    Edit     {} -> execute  dir command
    Obsolete {} -> execute dir command
    Rename   {} -> execute  dir command
    Retag    {} -> execute  dir command
    ChangeID {} -> execute dir command


execute :: Dir -> Command -> IO()

-- List the names of files matching the terms.
execute dir (List path Nothing sel) = getManyNotes dir sel >>=
  mapM_ (if path then putStrLn . notePath dir else printNote)

-- Apply command specified with --exec to files matching the terms.
execute dir (List _ (Just exec) sel) = do
  notes <- getManyNotes dir sel
  let cmd:args = words exec
  rawSystem cmd (args ++ map (notePath dir) notes) >>= \case
    ExitSuccess -> return ()
    code        -> print code

execute dir (Tags pop) = do
  ts <- countTags <$> getAllNotes dir
  if pop then mapM_ (uncurry (printf "%3d %s\n")) $ sortOn Down ts
         else mapM_ (putStrLn . snd) ts

execute dir (Cat noheaders sel) = do
  notes <- getManyNotes dir sel
  contents <- mapM (T.readFile . notePath dir) notes
  if noheaders
     then T.putStrLn $ T.intercalate "\n" contents
     else T.putStrLn $ T.intercalate "\n\n\n" $ zipWith (\n c -> header (pack $ noteFilename n) <> c) notes contents
  where
    header :: Text -> Text
    header s = s <> "\n" <> T.replicate (T.length s) "=" <> "\n"

-- | Edit the selected notes.
execute dir (Edit sel) = editNotes dir =<< getManyNotes dir sel


-- | Mark files as obsolete (prepend a '+' to the file name).
--   TODO make sure selection works as desired.
execute dir (Obsolete dry sel) = getManyNotes dir sel >>= modifyNotes dry f dir
  where
    f n = n { status = Obsoleted }

-- | Rename a single note.
execute dir (Rename dry sel nameParts) = getOneNote dir sel >>= mapM_ (modifyNote dry f dir)
  where
    f n = n { name = unwords nameParts }

-- | Change the tag of a file.
--   TODO make sure selection works as desired.
execute dir (Retag dry newTag sel) = getManyNotes dir sel >>= modifyNotes dry f dir
  where
    f n = n { tag = newTag }

-- | Change the ID of a file.
execute dir ChangeID {..} = do
  new <- case newID of
    Nothing -> makeAvailableID dir
    Just i  -> parseID i >>= firstAvailableID dir
  notes <- maybeToList <$> getOneNote dir selection
  modifyNotes dryrun (f new) dir notes
  where
    f new n = n { nid = new }


-- List bad files with headers.
execute dir (Check names refs) = do
  if names
    then do
      putStrLn "Badly named files"
      putStrLn "-----------------"
      checkNames dir
    else return ()
  putStrLn ""
  putStrLn "Files with duplicate identifiers"
  putStrLn "--------------------------------"
  checkDuplicateIDs dir
  if refs
    then do
      putStrLn ""
      putStrLn "Files with bad references"
      putStrLn "-------------------------"
      checkRefs dir
    else return ()


-- | Import a pre-existing file, optionally with a new title.
execute dir (Import (Just title) tag file) = importC' dir tag title =<< parseRelFile file
execute dir (Import Nothing tag file) = do
  file' <- parseRelFile file
  title <- unpack . filename' <$> file' -<.> ""
  importC' dir tag title file'



execute dir (New empty tag name) = do
  i <- makeAvailableID dir
  let note = Note Current i tag (unwords name) (Just ".txt")
  exec <- if empty then return "touch"  -- TODO: use Haskell actions for file creation instead.
                  else catchIOError (getEnv "EDITOR") defaultEditor
  let cmd:args = words exec
  rawSystem cmd (args ++ [notePath dir note]) >>= \case
    ExitSuccess -> checkinNote dir note >>= \case
      ExitSuccess -> printFilename note
      code        -> print code
    code        -> print code



modifyNote :: Run -> (Note -> Note) -> Dir -> Note -> IO ()
modifyNote run f dir = modifyNotes run f dir . pure

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
checkNames :: Dir -> IO ()
checkNames dir = mapM_ (T.putStrLn . filename') =<< getBadFiles dir
  -- Bad filenamess

checkDuplicateIDs :: Dir -> IO ()
checkDuplicateIDs dir = mapM_ (mapM_ (T.putStrLn . noteFilenameT)) . findDuplicateIDs =<< getAllNotes dir
  where
    findDuplicateIDs :: [Note] -> [[Note]]
    findDuplicateIDs = filter ((>1) . length) . groupBy equalIDs
    equalIDs n1 n2 = nid n1 == nid n2

-- List files with bad references.
checkRefs :: Dir -> IO ()
checkRefs _ = putStrLn "NOT IMPLEMENTED" -- TODO


importC' :: Dir -> String -> String -> Path Rel File -> IO ()
importC' dir tag title file = do
  i <- makeAvailableID dir
  let note = Note Current i tag title (Just $ fileExtension file)
  newfile <- noteAbsFile dir note
  copyFile file newfile
  checkinNote dir note >>= \case
    ExitSuccess -> printFilename note
    code        -> print code


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
