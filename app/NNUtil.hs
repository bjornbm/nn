{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module NNUtil where

import Control.Arrow ((&&&))
import Data.List (filter, group, init, sort, sortOn)
import qualified Data.List as L
import Data.Semigroup ((<>))
import Data.Text (Text, isSuffixOf, pack, unpack, splitOn)
import qualified Data.Text.IO as T (putStrLn)
import Data.Text.Normalize (normalize, NormalizationMode (NFC))
import Data.Time
import Data.Void
import Path ( Path (..), Abs (..), Rel (..), Dir (..), File (..)
            , parent, fromAbsDir, reldir
            , filename, fileExtension, parseAbsFile, fromAbsFile, fromRelFile
            , (</>), (-<.>)
            )
import Path.IO (listDir, renameFile)
import System.Process
import Text.Megaparsec
import Text.Megaparsec.Char


-- | Find files. We use the ASCII NULL terminated paths since file
-- names can contain @\n@ and would get split by @lines@.
mdfind :: Path Abs Dir -> [String] -> IO [Path Abs File]
mdfind dir args = fmap (sortOn filename) . mapM parseAbsFile . massage
  =<< readProcess "mdfind" (stdArgs dir ++ args) ""
  where
    stdArgs dir = [ "-onlyin", fromAbsDir dir , "-0" ]
    massage = map unpack
            . filter (not . isSuffixOf "~")   -- Vim backup file.
            . filter (not . isSuffixOf ",v")  -- RCS file.
            . init . splitOn "\0"  -- Null-terminated filenames.
            . pack

myfilter file = not (L.isSuffixOf "~"  file)   -- Vim backup file.
             && not (L.isSuffixOf ",v" file)  -- RCS file.

-- | List all files in the directory except for hidden files.
mdlist dir = sortOn filename . filter (myfilter . fromAbsFile) . snd <$> listDir dir

type Tag = Text
type ID = (String, String, String, String)
type Parser = Parsec Void Text

-- Regex patterns.
obsP :: Parser (Maybe Char)
obsP  = optional $ char '+'

idP :: Parser ID
idP = let sep = char '_' in do
  yyyy <-        count 4 digitChar
  mm   <- sep *> count 2 digitChar
  dd   <- sep *> count 2 digitChar
  hhmm <- sep *> count 4 digitChar
  return (yyyy,mm,dd,hhmm)

tagP :: Parser String
tagP  = char '-' *> some alphaNumChar <* char '-'  -- TODO åäöÅÄÖ don't work.

titleP :: Parser String
titleP = someTill anySingle (lookAhead $ try (eof    -- End of "good" file.
                           <|> char   '~'  *> eof    -- Unix (vim) backup file.
                           <|> string ",v" *> eof))  -- RCS file.

filePatternFull :: Parser String
filePatternFull = obsP *> idP *> tagP *> titleP <* eof
filePatternO :: Parser String
filePatternO = obsP *> idP *> tagP
filePattern :: Parser String
filePattern = idP *> tagP
filePatternID :: Text -> Parser String
filePatternID id = string id *> tagP
filePatternT :: Tag -> Parser String
filePatternT tag = unpack <$> (idP *> taggedP tag)
  where
    taggedP :: Tag -> Parser Text
    taggedP tag = char '-' *> string tag <* char '-'

-- | Extract tags from file names and count the number of uses of each tag.
-- TODO Use Megaparsec for the extraction to make tag delimiter flexible?
countTags :: [Path Abs File] -> [(Int, String)]
countTags = f . group . sort . map (takeWhile (/='-') . tail . dropWhile (/='-') . unpack . filename')
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
           , "-t-" ++ unpack msg   -- File description (first checkin).
           , "-m"  ++ unpack msg   -- Log message (second+ checkins).
           ]

-- | Rename a file as well as the corresponding RCS (,v) file.
-- The file is given a new revision documenting the old name.
renameRCS old new = do
  renameFile old new
  rcsold <- rcsfile old
  rcsnew <- rcsfile new
  renameFile rcsold rcsnew
  checkinForceMessage ("Renamed from \"" <> filename' old <> "\".") [new]
    where
      rcsfile :: Path Abs File -> IO (Path Abs File)
      rcsfile file = (parent file </> [reldir|RCS|] </> filename file) -<.> (fileExtension file ++ ",v")

-- | Get the filename (path removed) as @Text@.
  --
  -- The @Text@ is normalized because `mdfind` and @listDir@ does not use NFC
  -- normalisation for file names, so for example
  -- `length "Ö" == 2` in `mdfind` output.
filename' :: Path a File -> Text
filename' = normalize NFC . pack . fromRelFile . filename

-- | Print the filename (path removed).
printFilename :: Path Abs File -> IO ()
printFilename = T.putStrLn . filename'

