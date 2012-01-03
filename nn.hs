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

import Text.Regex.TDFA


{- TODO:
-  global option to specify NN_DIR.
-}

data NN = List { all :: Bool, exec :: Maybe String, terms :: [String] }
        | Cat { iD :: String }
        | Junk
        deriving (Show, Data, Typeable)


main = do
  mode <- cmdArgs (modes [listMode &= auto, catMode])
  dir <- getEnv "NN_HOME"
  setCurrentDirectory dir
  case mode of
    List _ _ _ -> list dir mode
    Cat _      -> print mode
    otherwise  -> list dir mode


listMode = List { exec = def &= help "Pass files as arguments to COMMAND" &= typ "COMMAND"
                , all = def &= help "Include obsoleted files in search"
                , terms = def &= args &= typ "SEARCH TERMS"
                }
catMode = Cat { iD = def &= args &= typ "FILE ID" }

getFiles dir [] = do
  processFiles <$> mdlist dir

getFiles dir terms = do
  processFiles <$> mdfind dir terms

-- List the names of files matching the terms.
list dir (List _ Nothing terms) = putStr . unlines =<< getFiles dir terms

-- Apply command specified with --exec to files matching the terms.
list dir (List _ (Just exec) terms) = do
  files <- getFiles dir terms
  let cmd:args = words exec
  code <- rawSystem cmd (args ++ files)
  case code of
    ExitSuccess -> return ()
    otherwise   -> print code


processFiles = sort . filter (=~ filePattern0) . fmap takeFileName
