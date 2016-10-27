module NNUtil where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.ICU.Normalize as T
import Data.Time
import GHC.IO.Encoding
import GHC.IO.Handle
import System.Directory
import System.FilePath
import System.Process


-- | Find files. We use the ASCII NULL terminated paths since file
-- names can contain @\n@ and would get split by @lines@.
mdfind dir args = do
  (_, Just h, _, _) <- createProcess (proc "mdfind" (stdArgs dir ++ args)){std_out=CreatePipe}
  print =<< getFileSystemEncoding
  print =<< getLocaleEncoding
  print =<< getForeignEncoding
  hSetEncoding h utf8
  x <- map takeFileName . endBy "\0" <$> hGetContents h
  mapM_ putStrLn x
  let y = map T.pack x
  print $ map (T.isNormalized T.NFC) y
  return $ map (T.unpack . T.normalize T.NFC) y
  where
    stdArgs dir = [ "-onlyin", dir , "-0" ]

mdfind dir args = map takeFileName . endBy "\0"
              <$> readProcess "mdfind" (stdArgs dir ++ args) ""
  where
    stdArgs dir = [ "-onlyin", dir , "-0" ]

-- | List all files in the directory except for hidden files.
mdlist dir = map takeFileName <$> getDirectoryContents dir


-- Regex patterns.
obsP  = "^\\+?"
idP   = "\\<[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{4}\\>"
tagP  = "-[a-zA-Z0-9åäöÅÄÖ]+-"  -- TODO åäöÅÄÖ don't work.
restP = ".+[^~]$"  -- Ignore unix (vim) backup files.
hiddenP = "^\\."   -- Ignore hidden files.
filePattern  = obsP ++ idP ++ tagP ++ restP
filePattern' = obsP ++ idP ++ tagP  -- Allows backup files to match.
filePattern0 = "^" ++ idP ++ tagP ++ restP  -- Don't match obsolete files.
filePatternT tag = "^" ++ idP ++ taggedP tag ++ restP
taggedP tag = "-" ++ tag ++ "-"

-- | Extract tags from file names and count the number of uses of each tag.
-- TODO Use regex for the extraction to make tag delimiter flexible?
countTags :: [FilePath] -> [(Int, String)]
countTags = f . group . sort . map (takeWhile (/='-') . tail . dropWhile (/='-'))
  where f = map (length &&& head)
  -- where f xs = zip (map length xs) (map head xs)

-- | Create an ID for a new file. Specifically a time stamp
-- based on the current local time with minute precision.
makeID :: IO String
makeID = do
  t  <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ formatTime undefined "%Y_%m_%d_%H%M" $ utcToLocalTime tz t
