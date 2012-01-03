{-# LANGUAGE DeriveDataTypeable #-}

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
-}

data NN = List { all :: Bool, exec :: Maybe String, terms :: [String] }
        | Cat { iD :: String }
        | Tags { popularity :: Bool }
        | Check { names :: Bool, references :: Bool }
        | Junk
        deriving (Show, Data, Typeable)


main = do
  mode <- cmdArgs (modes [listMode &= auto, catMode, tagsMode, checkMode])
  dir <- getEnv "NN_HOME"
  setCurrentDirectory dir
  case mode of
    List _ _ _ -> list mode
    Cat _      -> print mode
    Tags _     -> tags mode
    Check _ _  -> check mode
    otherwise  -> list mode



listMode = List { exec = def &= help "Pass files as arguments to COMMAND" &= typ "COMMAND"
                , all = def &= help "Include obsoleted files in search"
                , terms = def &= args &= typ "SEARCH TERMS"
                }
catMode = Cat { iD = def &= args &= typ "FILE ID" }
tagsMode = Tags { popularity = def &= help "Show and sort by the popularity of tags" }
checkMode = Check { names = def &= help "List badly named files"
                  , references = def &= help "List files containing bad file references"
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

-- List files with bad names.
check (Check True False) = do
  files <- mdlist
  mapM_ putStrLn $ sort
                 $ filter (/= ".")
                 $ filter (/= "..")
                 $ filter (not . (=~ filePattern'))
                 $ fmap takeFileName files

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



getFiles [] = do
  processFiles <$> mdlist

getFiles terms = do
  processFiles <$> mdfind terms

processFiles = sort . filter (=~ filePattern0) . fmap takeFileName
