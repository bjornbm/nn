{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NNUtil where

import Control.Arrow ((&&&))
import Control.Monad.Catch (MonadThrow)
import Data.List (filter, group, init, sort, sortOn)
import qualified Data.List as L
import Data.Maybe (isJust, fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, isSuffixOf, pack, unpack, splitOn)
import Data.Text.Normalize (normalize, NormalizationMode (NFC))
import Data.Time
import Data.Void
import Path ( Path (..), Abs (..), Rel (..), File (..)
            , parent, fromAbsDir, reldir
            , filename, fileExtension, parseAbsFile, parseAbsDir
            , fromAbsFile, parseRelFile, fromRelFile
            , (</>), (-<.>)
            )
import Path.IO (listDir, renameFile)
import System.Process
import Text.Megaparsec
import Text.Megaparsec.Char


-- | Find files. We use the ASCII NULL terminated paths since file
-- names can contain @\n@ and would get split by @lines@.
mdfind :: Dir -> [String] -> IO [Path Abs File]
mdfind dir args = fmap (sortOn filename) . mapM parseAbsFile . massage
  -- TODO sort [Note] instead of file paths?
  =<< readProcess "mdfind" (stdArgs dir ++ args) ""
  where
    stdArgs dir = [ "-onlyin", dir , "-0" ]
    massage = map unpack
            . filter (not . isSuffixOf "~")   -- Vim backup file.
            . filter (not . isSuffixOf ",v")  -- RCS file.
            . init . splitOn "\0"  -- Null-terminated filenames.
            . pack

myfilter file = not ("~"  `L.isSuffixOf` file)  -- Vim backup file.
             && not (",v" `L.isSuffixOf` file)  -- RCS file.

-- | List all files in the directory except for hidden files.
mdlist dir = do d <- parseAbsDir dir; sortOn filename . filter (myfilter . fromAbsFile) . snd <$> listDir d
  -- TODO sort [Note] instead of file paths?

type Dir = FilePath

data Status = Obsoleted | Current deriving (Eq, Ord, Show)
newtype ID = ID [String] deriving (Eq, Ord, Show)
type Tag = String
type Name = String
type Contents = Text
type Extension = String
data Note = Note
  { status :: Status
  , nid    :: ID
  , tag    :: Tag
  , name   :: Name
  , ext   :: Maybe Extension
  } deriving (Eq, Ord, Show)

notObsolete :: Note -> Bool
notObsolete = (/= Obsoleted) . status

hasTag :: Tag -> Note -> Bool
hasTag t = (t ==) . tag
hasID :: ID -> Note -> Bool
hasID  i = (i ==) . nid

-- | 'parts' breaks a String up into a list of parts, which were delimited
-- by underscores.
--
-- >>> parts "Lorem_ipsum_dolor"
-- ["Lorem","ipsum","dolor"]
splitParts :: String -> [String]
splitParts s = case dropWhile isSep s of
                  "" -> []
                  s' -> w : splitParts s''
                    where (w, s'') = break isSep s'
          where
            isSep = (== '_')

-- | Convert a note ID to its string representation.
  --
  -- >>> showID (ID ["2019", "03", "18", "1009"])
  -- "2019_03_18_1009"
showID :: ID -> String
showID (ID parts) = L.intercalate "_" parts

showStatus :: Status -> String
showStatus Obsoleted = "+"
showStatus _         = ""

-- | Create the filename of a note.
  --
  -- >>> noteFilename (Note Current (ID ["2019", "03", "18", "1009"]) "note" "The title" (Just ".txt"))
  -- "2019_03_18_1009-note-The title.txt"
  -- >>> noteFilename (Note Obsoleted (ID ["2019", "03", "18", "1009"]) "note" "The title" (Just ".md"))
  -- "+2019_03_18_1009-note-The title.md"
  -- >>> noteFilename (Note Obsoleted (ID ["2019", "03", "18", "1009"]) "note" "The title.md" Nothing)
  -- "+2019_03_18_1009-note-The title.md"
noteFilename :: Note -> String
noteFilename = unpack . noteFilenameT

noteFilenameT :: Note -> Text
noteFilenameT Note { .. }
  = (normalize NFC . pack) $ showStatus status
    <> showID nid <> "-" <> tag <> "-" <> name  -- The interesting parts
    <> fromMaybe "" ext

notePath :: Dir -> Note -> FilePath
-- TODO less secure?:
-- notePath dir note = dir System.FilePath.</> noteFilename note
notePath dir = maybe (error "Invalid filename or directory.") fromAbsFile
             . noteAbsFile dir

-- TODO: Consider adding extension in a principled manner with <.>?
noteRelFile :: MonadThrow m => Note -> m (Path Rel File)
noteRelFile = parseRelFile . noteFilename

-- TODO: Consider adding extension in a principled manner with <.>?
noteAbsFile :: MonadThrow m => Dir -> Note -> m (Path Abs File)
noteAbsFile dir note = (</>) <$> parseAbsDir dir <*> noteRelFile note


type Parser = Parsec Void Text

-- | Parse obsolete status.
--
-- >>> parseTest obsP $ pack "+name"
-- Obsoleted
--
-- >>> parseTest obsP $ pack "name"
-- Current
obsP :: Parser Status
obsP  = maybe Current (const Obsoleted) <$> optional (char '+')

idP :: Parser ID
idP = let sep = char '_' in do
  yyyy <-        count 4 digitChar
  mm   <- sep *> count 2 digitChar
  dd   <- sep *> count 2 digitChar
  hhmm <- sep *> count 4 digitChar
  return $ ID [yyyy, mm, dd, hhmm]

tagP :: Parser String
tagP  = char '-' *> some alphaNumChar <* char '-'  -- TODO åäöÅÄÖ don't work.

backupP :: Parser ()
backupP = char '~' *> eof
rcsP :: Parser ()
rcsP = string ",v" *> eof

-- | Parse note title/name.
--
-- >>> parseTest titleP $ pack "monkey business"
-- "monkey business"
--
-- >>> parseTest titleP $ pack "monkey business.txt"
-- "monkey business"
--
-- >>> parseTest titleP $ pack "monkey business.md.txt"
-- "monkey business.md"
--
-- TODO Why the following?
-- >>> parseTest titleP $ pack "monkey business~"
-- "monkey business"
--
-- >>> parseTest titleP $ pack "monkey business.txt~"
-- "monkey business"
--
-- TODO Why the following?
-- >>> parseTest titleP $ pack "monkey business,v"
-- "monkey business"
--
-- >>> parseTest titleP $ pack "monkey business.txt.md,v"
-- "monkey business.txt"
--
titleP :: Parser String
titleP = someTill anySingle (lookAhead $ try (optional extP <* endP))

endP = eof <|> backupP <|> rcsP

extP :: Parser (Maybe Extension)
extP = optional $ ("." <>) <$> (char '.' *> many alphaNumChar)

-- | Parse a Note from a String (normally a file name)
  -- TODO: Separate extension from title.
  --
  -- >>> parseTest noteParser (pack "+2019_03_18_1009-note-The title.md")
  -- Note Obsoleted (ID ["2019","03","18","1009"]) "note" "The title" (Just ".md")
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-The title.txt")
  -- Note Current (ID ["2019","03","18","1009"]) "note" "The title" (Just ".txt")
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-www.klintenas.se.txt")
  -- Note Current (ID ["2019","03","18","1009"]) "note" "www.klintenas.se" (Just ".txt")
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-www.klintenas.se.txt~")
  -- Note Current (ID ["2019","03","18","1009"]) "note" "www.klintenas.se" (Just ".txt")
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-The title")
  -- Note Current (ID ["2019","03","18","1009"]) "note" "The title" Nothing
noteParser :: Parser Note
noteParser = Note <$> obsP <*> idP <*> tagP <*> titleP <*> extP <* endP

-- | Extract tags from file names and count the number of uses of each tag.
  -- TODO Use Megaparsec for the extraction to make tag delimiter flexible?
countTags :: [Note] -> [(Int, String)]
countTags = map (length &&& head) . group . sort . map tag

-- | Create an ID for a new file. Specifically a time stamp
  -- based on the current local time with minute precision.
makeID :: IO ID
makeID = localTimeToID <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

localTimeToID :: LocalTime -> ID
localTimeToID t = ID $ map (\fmt -> formatTime undefined fmt t) ["%Y", "%m", "%d", "%H%M"]


-- | Check in notes with RCS. Use default description/message.
--checkinNotes :: Dir -> [Note] -> IO ExitCode
checkinNotes dir notes = rawSystem "rcs" (args ++ map (notePath dir) notes)
  where
    args = [ "ci"   -- Check in.
           , "-l"   -- Check out the file locked (with write permissions).
           , "-t-\"Created by nn.\""  -- File description (first checkin).
           , "-m\"Updated by nn.\""   -- Log message (second+ checkins).
           ]

-- | Check in note with RCS. Use default description/message.
--checkinNote :: Dir -> Note -> IO ExitCode
checkinNote dir note = checkinNotes dir [note]

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
renameRCS dir oldNote newNote = do
  old <- noteAbsFile dir oldNote
  new <- noteAbsFile dir newNote
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
printFilename :: Note -> IO ()
printFilename = putStrLn . noteFilename

-- | Show the note
showNote :: Note -> String
showNote Note { .. } = showStatus status <> showID nid <> " [" <> tag <> "] " <> name

-- | Print the note
printNote :: Note -> IO ()
printNote = putStrLn . showNote

