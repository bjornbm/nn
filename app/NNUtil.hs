module NNUtil where

import Control.Applicative
import Data.List
import Data.List.Split
import Data.Time
import System.Directory
import System.Exit
import System.FilePath
import System.Process


-- | Find files. We use the ASCII NULL terminated paths since file
-- names can contain @\n@ and would get split by @lines@.
mdfind args = do
  dir <- getCurrentDirectory
  map takeFileName . endBy "\0" <$> readProcess "mdfind" (stdArgs dir ++ args) ""
  where
    stdArgs dir = [ "-onlyin", dir
                  , "-0"
                  ]
mdfind' dir args = do
  map takeFileName . endBy "\0" <$> readProcess "mdfind" (stdArgs dir ++ args) ""
  where
    stdArgs dir = [ "-onlyin", dir
                  , "-0"
                  ]

-- | List all files in the directory except for hidden files.
mdlist = map takeFileName <$> (getDirectoryContents =<< getCurrentDirectory)


-- Regex patterns.
obsP  = "^\\+?"
idP   = "\\<[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{4}\\>"
tagP  = "-[a-zA-Z0-9_.]+-"
restP = ".+[^~]$"  -- Ignore unix (vim) backup files.
filePattern  = obsP ++ idP ++ tagP ++ restP
filePattern' = obsP ++ idP ++ tagP  -- Allows backup files to match.
filePattern0 = "^" ++ idP ++ tagP ++ restP  -- Don't match obsolete files.

-- | Extract tags from file names and count the number of uses of each tag.
countTags :: [FilePath] -> [(Int, String)]
countTags = f . group . sort . map (takeWhile (/='-') . tail . dropWhile (/='-'))
  where f xs = zip (map length xs) (map head xs)

-- | Create an ID for a new file. Specifically a time stamp
-- based on the current local time with minute precision.
makeID :: IO String
makeID = do
  t  <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ formatTime undefined ("%Y_%m_%d_%H%M") $ utcToLocalTime tz t
