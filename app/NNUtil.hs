{-# LANGUAGE QuasiQuotes #-}

module NNUtil where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.List
import Data.List.Split
import Data.Text (pack, unpack)
import Data.Text.Normalize (normalize, NormalizationMode (NFC))
import Data.Time
import GHC.IO.Encoding
import GHC.IO.Handle
import Path ( Path (..), Abs (..), Rel (..), Dir (..), File (..)
            , parent, fromAbsDir, reldir
            , filename, fileExtension, parseAbsFile, fromAbsFile, fromRelFile
            , (</>), (-<.>)
            )
import Path.IO (listDir, renameFile)
import System.Process


-- | Find files. We use the ASCII NULL terminated paths since file
-- names can contain @\n@ and would get split by @lines@.
mdfind :: Path Abs Dir -> [String] -> IO [Path Abs File]
mdfind dir args = mapM parseAbsFile . massage
  =<< readProcess "mdfind" (stdArgs dir ++ args) ""
  where
    stdArgs dir = [ "-onlyin", fromAbsDir dir , "-0" ]
    massage = endBy "\0"  -- Null-terminated filenames.
            -- Normalize because `mdfind` does not use NFC
            -- normalisation for file names, so for example
            -- `length "Ö" == 2` in `mdfind` output..
            . unpack . normalize NFC . pack

-- | List all files in the directory except for hidden files.
mdlist dir = snd <$> listDir dir


-- Regex patterns.
obsP  = "^\\+?"
idP   = "\\<[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{4}\\>"
tagP  = "-[a-zA-Z0-9åäöÅÄÖ]+-"  -- TODO åäöÅÄÖ don't work.
restP = ".+[^~]$"  -- Ignore unix (vim) backup files.
hiddenP = "^\\."   -- Ignore hidden files.
rcsP = ",v$"
filePattern  = obsP ++ idP ++ tagP ++ restP
filePattern' = obsP ++ idP ++ tagP  -- Allows backup files to match.
filePattern0 = "^" ++ idP ++ tagP ++ restP  -- Don't match obsolete files.
filePatternT tag = "^" ++ idP ++ taggedP tag ++ restP
taggedP tag = "-" ++ tag ++ "-"

-- | Extract tags from file names and count the number of uses of each tag.
-- TODO Use regex for the extraction to make tag delimiter flexible?
countTags :: [Path Abs File] -> [(Int, String)]
countTags = f . group . sort . map (takeWhile (/='-') . tail . dropWhile (/='-'))
          . map fromAbsFile
  where f = map (length &&& head)
  -- where f xs = zip (map length xs) (map head xs)

-- | Create an ID for a new file. Specifically a time stamp
-- based on the current local time with minute precision.
makeID :: IO String
makeID = do
  t  <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ formatTime undefined "%Y_%m_%d_%H%M" $ utcToLocalTime tz t

-- | Check in file with RCS. Use default description/message.
checkin files = rawSystem "rcs" (args ++ map fromAbsFile files)
  where
    args = [ "ci"   -- Check in.
           , "-l"   -- Check out the file locked (with write permissions).
           , "-t-\"Created by nn.\""  -- File description (first checkin).
           , "-m\"Updated by nn.\""   -- Log message (second+ checkins).
           ]

-- | Check in file with RCS. The checkin is forced even if the file
-- has not been changed which ensures the log message is written.
checkinForceMessage msg files = rawSystem "rcs" (args ++ map fromAbsFile files)
  where
    args = [ "ci"   -- Check in.
           , "-l"   -- Check out the file locked (with write permissions).
           , "-f"   -- Force new version.
           , "-t-" ++ msg   -- File description (first checkin).
           , "-m"  ++ msg   -- Log message (second+ checkins).
           ]

-- | Rename a file as well as the corresponding RCS (,v) file.
-- The file is given a new revision documenting the old name.
renameRCS old new = do
  renameFile old new
  rcsold <- rcsfile old
  rcsnew <- rcsfile new
  renameFile rcsold rcsnew
  checkinForceMessage ("Renamed from \"" ++ filename' old ++"\".") [new]
    where
      rcsfile :: Path Abs File -> IO (Path Abs File)
      rcsfile file = (parent file </> [reldir|RCS|] </> filename file) -<.> (fileExtension file ++ ",v")

-- | Get the filename (path removed) as a string.
filename' :: Path a File -> String
filename' = fromRelFile . filename

-- | Print the filename (path removed).
printFilename :: Path Abs File -> IO ()
printFilename = putStrLn . filename'

