{-# LANGUAGE DisambiguateRecordFields #-}

import Control.Applicative
import System.IO.Error(catchIOError)
import Data.List (intercalate, sort, sortBy)
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
-  nn 2010 doesn't find anything. In fact, mdfind will not find anything with 2010 in it!!
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
  command <- parseCommand
  dir <- getEnv "NN_HOME"
  case command of
    List  {} -> list  dir command  -- TODO don't cd!
    Cat   {} -> cat   dir command
    Tags  {} -> tags  dir command
    Check {} -> check dir command
    Save  {} -> save  dir command
    New   {} -> new   dir command
    Edit  {} -> edit  dir command
    _        -> list  dir command


-- List the names of files matching the terms.
list dir (None terms) = mapM_ putStrLn =<< getFiles dir Nothing terms
list dir (List _ Nothing tag terms) = mapM_ putStrLn =<< getFiles dir tag terms

-- Apply command specified with --exec to files matching the terms.
list dir (List _ (Just exec) tag terms) = do
  files <- getFiles dir tag terms
  let cmd:args = words exec
  code <- rawSystem cmd (args ++ files)
  case code of
    ExitSuccess -> return ()
    _           -> print code

tags dir (Tags pop) = do
  ts <- countTags <$> getFiles dir Nothing []
  if pop then mapM_ (uncurry (printf "%3d %s\n")) $ reverseSort ts
         else mapM_ (putStrLn . snd) ts
  where
    reverseSort = sortBy (flip compare)

cat dir (Cat noheaders id) = do
  files <- getFiles dir Nothing ["name:"++id]  -- TODO not solid. TODO use tag
  contents <- mapM (readFile . (dir </>)) files
  if noheaders
     then putStr $ intercalate "\n" contents
     else putStr $ intercalate "\n\n\n" $ zipWith (\f c -> header f ++ c) files contents
  where
    header s = s ++ "\n" ++ replicate (length s) '=' ++ "\n"

edit dir (Edit id) = do
  files <- processFiles Nothing <$> mdfind' dir ["name:"++id]  -- TODO not solid.
  exec <- catchIOError (getEnv "EDITOR") defaultEditor
  let cmd:args = words exec
  code <- rawSystem cmd (args ++ map (dir </>) files)
  case code of
    ExitSuccess -> return ()
    _           -> print code


-- List files with bad names.
check :: FilePath -> Command -> IO ()
check dir (Check True False) = do
  files <- mdlist dir
  mapM_ putStrLn $ sort
                 $ filter (/= ".")
                 $ filter (/= "..")
                 $ filter (not . (=~ hiddenP))
                 $ filter (not . (=~ filePattern'))
                 files

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
    _           -> print code

getFiles dir Nothing    []    = processFiles Nothing    <$> mdlist dir
getFiles dir Nothing    terms = processFiles Nothing    <$> mdfind dir terms
getFiles dir (Just tag) terms = processFiles (Just tag) <$> mdfind dir (tag:terms)

processFiles :: Maybe String -> [FilePath] -> [String]
processFiles Nothing = sort . filter (=~ filePattern0)
processFiles (Just tag) = sort . filter (=~ filePatternT tag)
