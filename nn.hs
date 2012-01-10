{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DisambiguateRecordFields #-}

import Control.Applicative
import Data.List hiding (all)
import System
import System.FilePath
import System.Process
import NNUtil
import System.Console.CmdArgs
import System.Directory
import Prelude hiding (all)
import Text.Printf

import Text.Regex.TDFA


{- TODO:
-  global option to specify NN_DIR.
-  Shouldn't change cwd when usind list --exec!
-}

data NN = List { all :: Bool, exec :: Maybe String, terms :: [String] }
        | Cat { id :: String }
        | Tags { popularity :: Bool }
        | Check { names :: Bool, references :: Bool }
        | Save { rename :: Maybe String, tag :: String, file :: String }
        | New { tag :: String, name :: [String] }
        | Junk
        deriving (Show, Data, Typeable)


main = do
  mode <- cmdArgs (modes [listMode &= auto, catMode, tagsMode, checkMode, saveMode, newMode])
  dir <- getEnv "NN_HOME"
  case mode of
    List _ _ _ -> setCurrentDirectory dir >> list mode
    Cat _      -> setCurrentDirectory dir >> cat mode
    Tags _     -> setCurrentDirectory dir >> tags mode
    Check _ _  -> setCurrentDirectory dir >> check mode
    Save _ _ _ -> save dir mode
    New _ _    -> new dir mode
    otherwise  -> setCurrentDirectory dir >> list mode



listMode = List { exec = def &= help "Pass files as arguments to COMMAND" &= typ "COMMAND"
                , all = def &= help "Include obsoleted files in search"
                , terms = def &= args &= typ "SEARCH TERMS"
                }
catMode = Cat { id = def &= args &= typ "FILE ID" }
tagsMode = Tags { popularity = def &= help "Show and sort by the popularity of tags" }
checkMode = Check { names = def &= help "List badly named files"
                  , references = def &= help "List files containing bad file references"
                  }
saveMode = Save { rename = def &= help "Save with descriptive name NAME" &= typ "NAME"
                , tag = def &= typ "TAG" &= argPos 0
                , file = def &= typ "FILE" &= argPos 1
                }
newMode = New { tag = def &= typ "TAG" &= argPos 0
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

cat (Cat id) = do
  files <- getFiles ["name:"++id]
  contents <- mapM readFile files  -- TODO doesn't work with unicode filenames. Fixed in 7.2.1?
  putStr $ unlines $ zipWith (\f c -> header f ++ c) files contents
  where
    header s = s ++ "\n" ++ take (length s) (repeat '=') ++ "\n"

-- List files with bad names.
check (Check True False) = do
  files <- mdlist
  mapM_ putStrLn $ sort
                 $ filter (/= ".")
                 $ filter (/= "..")
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

new dir (New tag name) = do
  id <- makeID
  let newfile = id ++ "-" ++ tag ++ "-" ++ unwords name <.> ".txt"
  cmd <- catch (getEnv "EDITOR") (const (return "vi"))
  code <- rawSystem cmd [dir </> newfile]
  case code of
    ExitSuccess -> putStrLn newfile
    otherwise   -> print code

getFiles [] = do
  processFiles <$> mdlist

getFiles terms = do
  processFiles <$> mdfind terms

processFiles = sort . filter (=~ filePattern0)
