{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import System.IO.Error (catchIOError)
import Data.List (intercalate, sort, sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (pack)
import System.Environment (getEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>), (<.>), takeFileName, takeExtension)
import System.Process (rawSystem)
import NNUtil
import System.Directory (setCurrentDirectory, copyFile)
import Text.Printf (printf)
import Wybor

import Text.Regex.TDFA

main = do
  dir   <- getEnv "NN_HOME"
  files <- processFiles (Just "job") <$> mdlist dir
  file  <- select . fromTexts . ("New" :|) . map pack $ files
  print file

processFiles :: Maybe String -> [FilePath] -> [String]
processFiles Nothing = sort . filter (=~ filePattern0)
processFiles (Just tag) = sort . filter (=~ filePatternT tag)
