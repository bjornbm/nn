{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module NNUtil where

import Control.Arrow ((&&&))
import Control.Monad.Catch (MonadThrow)
import Data.List (filter, group, init, sort, sortOn)
import qualified Data.List as L
import Data.Maybe (isJust)
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
import System.FilePath (splitExtension)
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
type Title = String
type Contents = Text
type Extension = String
data Note = Note Status ID Tag Title Extension deriving (Eq, Ord, Show)

notObsolete (Note Obsoleted _ _ _ _) = False
notObsolete _                        = True
hasTag tag  (Note _ _ t _ _) = t == tag
hasID id    (Note _ i _ _ _) = i == id

-- | 'parts' breaks a String up into a list of parts, which were delimited
-- by underscores.
--
-- >>> parts "Lorem_ipsum_dolor"
-- ["Lorem","ipsum","dolor"]
parts :: String -> [String]
parts s = case dropWhile isSep s of
            "" -> []
            s' -> w : parts s''
                    where (w, s'') = break isSep s'
          where
            isSep = (== '_')

-- | Convert a note ID to its string representation.
  --
  -- >>> showID (ID ["2019", "03", "18", "1009"])
  -- "2019_03_18_1009"
showID :: ID -> String
showID (ID parts) = L.intercalate "_" parts

-- | Create the filename of a note.
  --
  -- >>> noteFilename (Note Current (ID ["2019", "03", "18", "1009"]) "note" "The title" ".txt")
  -- "2019_03_18_1009-note-The title.txt"
  -- >>> noteFilename (Note Obsoleted (ID ["2019", "03", "18", "1009"]) "note" "The title" ".md")
  -- "+2019_03_18_1009-note-The title.md"
  -- >>> noteFilename (Note Obsoleted (ID ["2019", "03", "18", "1009"]) "note" "The title.md" "")
  -- "+2019_03_18_1009-note-The title.md"
noteFilename :: Note -> String
noteFilename = unpack . noteFilenameT

noteFilenameT :: Note -> Text
noteFilenameT (Note status id tag title ext)
  = (normalize NFC . pack) $
  obsoleted status
  <> showID id <> "-" <> tag <> "-" <> title  -- The interesting parts
  <> (if null ext then "" else ext)
  where
    obsoleted Obsoleted = "+"
    obsoleted _         = ""

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

-- | Parse note title/name.
--
-- >>> parseTest titleP $ pack "monkey business"
-- "monkey business"
--
-- >>> parseTest titleP $ pack "monkey business.txt"
-- "monkey business.txt"
--
-- TODO>>> parseTest titleP $ pack "monkey business.txt"
-- "monkey business"
--
-- TODO>>> parseTest titleP $ pack "monkey business.md.txt"
-- "monkey business.md"
--
-- TODO Why the following?
-- >>> parseTest titleP $ pack "monkey business~"
-- "monkey business"
--
-- TODO Why the following?
-- >>> parseTest titleP $ pack "monkey business,v"
-- "monkey business"
--
titleP :: Parser String
titleP = someTill anySingle (lookAhead $ try (eof    -- End of "good" file.
                           <|> char   '~'  *> eof    -- Unix (vim) backup file.
                           <|> string ",v" *> eof))  -- RCS file.

-- | Parse a Note from a String (normally a file name)
  -- TODO: Separate extension from title.
  --
  -- >>> parseTest noteParser (pack "+2019_03_18_1009-note-The title.md")
  -- Note Obsoleted (ID ["2019","03","18","1009"]) "note" "The title" ".md"
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-The title.txt")
  -- Note Current (ID ["2019","03","18","1009"]) "note" "The title" ".txt"
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-www.klintenas.se.txt")
  -- Note Current (ID ["2019","03","18","1009"]) "note" "www.klintenas.se" ".txt"
noteParser :: Parser Note
noteParser = fixExt
         <$> (Note <$> obsP <*> idP <*> tagP <*> titleP <*> pure undefined)
  where
    -- TODO: ugly to not do this properly with a parser.
    fixExt (Note o i t ne _) = uncurry (Note o i t) $ splitExtension ne

-- | Extract tags from file names and count the number of uses of each tag.
  -- TODO Use Megaparsec for the extraction to make tag delimiter flexible?
countTags :: [Note] -> [(Int, String)]
countTags = f . group . sort . map getTag
  where f = map (length &&& head)
        getTag (Note _ _ t _ _) = t
  -- where f xs = zip (map length xs) (map head xs)

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

