{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DisambiguateRecordFields #-}

import Control.Applicative
import System.IO.Error(catchIOError)
import Data.List (intercalate, sort)
import System.Environment (getEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>), (<.>), takeFileName, takeExtension)
import System.Process (rawSystem)
import NNUtil
import System.Directory (setCurrentDirectory, copyFile)
import Prelude hiding (all)
import Text.Printf (printf)

import Text.Regex.TDFA

import Options


defaultEditor = const (return "vim")

{- TODO:
-  Global option to specify NN_DIR.
-  Shouldn't change cwd when usind list --exec!
-  Option to print full file name including path (in quotes or with with escaped spaces?)
-  Integrate with git (if NN_DIR is a repo): add new files and commit changes after edit. Option to push automatically?
-  nn edit based on search term rather than ID?
-  nn cat works strange (not like search); use ID or search term??
-  nn rename command, takes -i
-  nn redate command, takes -i and optional new date (otherwise now)
-  nn pandoc command?
-  nn pretty command? (| pandoc --smart --to=plain)
-  nn obsolete command, takes -i and adds + in front of ID
-  nn path command, returns full path of matching files
-  Consistently use search term and or -i for edit, cat, etc.
-  Allow multiple tags separated by underscore {ID}-tag1_tag2-{TITLE}
-  nn -t tag option for searching by tag
-  nn tag command to add tag to notes
-  nn rmtag command to remove tag from notes
-  nn touch command to update ID to today's time (use `git mv` if repo!)
-  Change ID format from 2014_05_20_1738 to 20140520T1738?
-  nn stats command to list total number of current and obsolete files, etc?
-  nn check files without tags and without titles
-}


main = do
  mode <- parseCommand
  dir <- getEnv "NN_HOME"
  case mode of
    List _ _ _ _ -> setCurrentDirectory dir >> list mode  -- TODO don't cd!
    Cat _ _      -> setCurrentDirectory dir >> cat mode
    Tags _       -> setCurrentDirectory dir >> tags mode
    Check _ _    -> setCurrentDirectory dir >> check mode
    Save _ _ _   -> save dir mode
    New _ _ _    -> new dir mode
    Edit _       -> edit dir mode
    otherwise    -> setCurrentDirectory dir >> list mode


-- List the names of files matching the terms.
list (None terms) = mapM_ putStrLn =<< getFiles Nothing terms
list (List _ Nothing tag terms) = mapM_ putStrLn =<< getFiles tag terms

-- Apply command specified with --exec to files matching the terms.
list (List _ (Just exec) tag terms) = do
  files <- getFiles tag terms
  let cmd:args = words exec
  code <- rawSystem cmd (args ++ files)
  case code of
    ExitSuccess -> return ()
    otherwise   -> print code

tags (Tags pop) = do
  ts <- countTags <$> getFiles Nothing []
  if pop then mapM_ (uncurry (printf "%3d %s\n")) $ reverse $ sort ts
         else mapM_ putStrLn $ map snd ts

cat (Cat noheaders id) = do
  files <- getFiles Nothing ["name:"++id]  -- TODO not solid. TODO use tag
  contents <- mapM readFile files  -- TODO doesn't work with unicode filenames. Fixed in 7.2.1?
  if noheaders
     then putStr $ intercalate "\n" $ contents
     else putStr $ intercalate "\n\n\n" $ zipWith (\f c -> header f ++ c) files contents
  where
    header s = s ++ "\n" ++ take (length s) (repeat '=') ++ "\n"

edit dir (Edit id) = do
  files <- processFiles Nothing <$> mdfind' dir ["name:"++id]  -- TODO not solid.
  exec <- catchIOError (getEnv "EDITOR") defaultEditor
  let cmd:args = words exec
  code <- rawSystem cmd (args ++ map (dir </>) files)
  case code of
    ExitSuccess -> return ()
    otherwise   -> print code


-- List files with bad names.
check (Check True False) = do
  files <- mdlist
  mapM_ putStrLn $ sort
                 $ filter (/= ".")
                 $ filter (/= "..")
                 $ filter (not . (=~ hiddenP))
                 $ filter (not . (=~ filePattern'))
                 $ files

-- List files with bad references.
check (Check False True) = do
  putStrLn "NOT IMPLEMENTED" -- TODO

-- List bad files with headers.
check (Check False False) = do
  putStrLn "Badly named files"
  putStrLn "-----------------"
  check (Check True False)
  putStrLn ""
  putStrLn "Files with duplicate identifiers"
  putStrLn "--------------------------------"
  putStrLn "NOT IMPLEMENTED" -- TODO
  putStrLn ""
  putStrLn "Files with bad references"
  putStrLn "-------------------------"
  check (Check False True)

check (Check True True) = do
  check (Check True False)
  check (Check False True)


save dir (Save Nothing tag file) = save' dir tag file (takeFileName file)
save dir (Save (Just name) tag file) = save' dir tag file (name <.> takeExtension file)

save' dir tag file name = do
  id <- makeID
  let newfile = id ++ "-" ++ tag ++ "-" ++ name
  copyFile file (dir </> newfile)
  putStrLn newfile

new dir (New empty tag name) = do
  id <- makeID
  let newfile = id ++ "-" ++ tag ++ "-" ++ unwords name <.> ".txt"
  cmd <- if empty then return "touch"  -- TODO: use Haskell actions for file creation instead.
                  else catchIOError (getEnv "EDITOR") defaultEditor
  code <- rawSystem cmd [dir </> newfile]
  case code of
    ExitSuccess -> putStrLn newfile
    otherwise   -> print code

getFiles Nothing    []    = processFiles Nothing <$> mdlist
getFiles Nothing    terms = processFiles Nothing <$> mdfind terms
getFiles (Just tag) terms = processFiles (Just tag) <$> mdfind (tag:terms)

processFiles :: Maybe String -> [FilePath] -> [String]
processFiles Nothing = sort . filter (=~ filePattern0)
processFiles (Just tag) = sort . filter (=~ filePatternT tag)
