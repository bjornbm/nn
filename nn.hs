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
        | Junk
        deriving (Show, Data, Typeable)


main = do
  mode <- cmdArgs (modes [listMode &= auto, catMode, tagsMode])
  dir <- getEnv "NN_HOME"
  setCurrentDirectory dir
  case mode of
    List _ _ _ -> list mode
    Cat _      -> print mode
    Tags _     -> tags mode
    otherwise  -> list mode



listMode = List { exec = def &= help "Pass files as arguments to COMMAND" &= typ "COMMAND"
                , all = def &= help "Include obsoleted files in search"
                , terms = def &= args &= typ "SEARCH TERMS"
                }
catMode = Cat { iD = def &= args &= typ "FILE ID" }
tagsMode = Tags { popularity = def &= help "Show and sort by the popularity of tags" }

-- List the names of files matching the terms.
list (List _ Nothing terms) = putStr . unlines =<< getFiles terms

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
         else putStr $ unlines $ map snd ts


getFiles [] = do
  processFiles <$> mdlist

getFiles terms = do
  processFiles <$> mdfind terms

processFiles = sort . filter (=~ filePattern0) . fmap takeFileName
