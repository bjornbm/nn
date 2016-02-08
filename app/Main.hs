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
import System.Console.CmdArgs
import System.Directory (setCurrentDirectory, copyFile)
import Prelude hiding (all)
import Text.Printf (printf)

import Text.Regex.TDFA


defaultEditor = const (return "vim")

{- TODO:
-  global option to specify NN_DIR.
-  Shouldn't change cwd when usind list --exec!
-}

data NN = List { all :: Bool, exec :: Maybe String, terms :: [String] }
        | Cat { noheaders :: Bool, id :: String }
        | Edit { id :: String }
        | Tags { popularity :: Bool }
        | Check { names :: Bool, references :: Bool }
        | Save { rename :: Maybe String, tag :: String, file :: String }
        | New { empty :: Bool, tag :: String, name :: [String] }
        | Junk
        deriving (Show, Data, Typeable)


main = do
  mode <- cmdArgs (modes [listMode &= auto, catMode, editMode, tagsMode, checkMode, newMode, saveMode])
  dir <- getEnv "NN_HOME"
  case mode of
    List _ _ _ -> setCurrentDirectory dir >> list mode  -- TODO don't cd!
    Cat _ _    -> setCurrentDirectory dir >> cat mode
    Tags _     -> setCurrentDirectory dir >> tags mode
    Check _ _  -> setCurrentDirectory dir >> check mode
    Save _ _ _ -> save dir mode
    New _ _ _  -> new dir mode
    Edit _     -> edit dir mode
    otherwise  -> setCurrentDirectory dir >> list mode



listMode = List { exec = def &= help "Pass files as arguments to COMMAND" &= typ "COMMAND"
                , all = def &= help "Include obsoleted files in search"
                , terms = def &= args &= typ "SEARCH TERMS"
                }
catMode = Cat { noheaders = def &= help "Do not include headers in output"
              , id = def &= args &= typ "FILE ID" }
editMode = Edit { id = def &= args &= typ "FILE ID" }  -- TODO: Why doesn't this behave identically to the previous line?
                                                       -- See the help message generate by cmdargs. Looks like a bug in
                                                       -- cmdargs to me, where only the first occurence turn out OK.
tagsMode = Tags { popularity = def &= help "Show and sort by the popularity of tags" }
checkMode = Check { names = def &= help "List badly named files"
                  , references = def &= help "List files containing bad file references"
                  }
saveMode = Save { rename = def &= help "Save with descriptive name NAME" &= typ "NAME"
                , tag = def &= typ "TAG" &= argPos 0  -- TODO: See editMode TODO.
                , file = def &= typ "FILE" &= argPos 1
                }
newMode = New { empty = def &= help "Create empty file"
              , tag = def &= typ "TAG" &= argPos 0
              , name = def &= typ "NAME" &= args
              }

-- List the names of files matching the terms.
list (List _ Nothing terms) = mapM_ putStrLn =<< getFiles terms

-- Apply command specified with --exec to files matching the terms.
list (List _ (Just exec) terms) = do
  files <- getFiles terms
  let cmd:args = words exec
  code <- rawSystem cmd (args ++ files)
  case code of
    ExitSuccess -> return ()
    otherwise   -> print code

tags (Tags pop) = do
  ts <- countTags <$> getFiles []
  if pop then mapM_ (uncurry (printf "%3d %s\n")) $ reverse $ sort ts
         else mapM_ putStrLn $ map snd ts

cat (Cat noheaders id) = do
  files <- getFiles ["name:"++id]  -- TODO not solid.
  contents <- mapM readFile files  -- TODO doesn't work with unicode filenames. Fixed in 7.2.1?
  if noheaders
     then putStr $ intercalate "\n" $ contents
     else putStr $ intercalate "\n\n\n" $ zipWith (\f c -> header f ++ c) files contents
  where
    header s = s ++ "\n" ++ take (length s) (repeat '=') ++ "\n"

edit dir (Edit id) = do
  files <- processFiles <$> mdfind' dir ["name:"++id]  -- TODO not solid.
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

getFiles [] = do
  processFiles <$> mdlist

getFiles terms = do
  processFiles <$> mdfind terms

processFiles = sort . filter (=~ filePattern0)