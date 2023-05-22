{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Util where

import Control.Arrow ((&&&))
import Control.Monad.Catch (MonadThrow)
import Data.Char (isLower)
import Data.List (group, sort, sortOn)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text, isSuffixOf, pack, unpack, splitOn)
import Data.Text.Normalize (normalize, NormalizationMode (NFC))
import Data.Time
import Data.Void
import Path ( parent, reldir
            , filename, fileExtension, parseAbsFile
            , fromAbsFile, parseRelFile, fromRelFile
            , (</>), replaceExtension
            )
import qualified Path as P
import Path.IO (listDir, renameFile)
import System.Exit (ExitCode)
import System.Process
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf (printf)


data SearchTool = Ack | Ag | Find

instance Show SearchTool
  where
    show Ack = "ack"
    show Ag = "ag"
    show Find = "find"


searchOptions :: SearchTool -> Dir -> String -> [String]
searchOptions Ack dir s =
    [ "--nofilter"  -- Don't go bananas from (empty) pipe on stdin (`readProcess` artifact).
    , "--smart-case"
    , "--print0"
    , "--ignore-dir=RCS"
    , "--nocolor"
    , "--literal"
    , "-n"  -- Don't bother with recursing into subdirectories
    , "-g"  -- List matching files instead of searching file contents
    , s
    , P.fromAbsDir dir
    ]
searchOptions Ag dir s = "--parallel"  -- same purpose as `--nofilter` in ack.
    : tail (searchOptions Ack dir s)   -- TODO: ugly.
searchOptions Find dir s =
    [ P.fromAbsDir dir
    , if all isLower s then "-iname" else "-name"  -- "smart" case-sensitivity.
    , "*" <> s <> "*"  -- glob around search term.
    , "!", "-name", "*~"  -- Ignore vim backup files.
    , "!", "-name", "*,v"  -- Ignore RCS files.
    , "-print0"
    , "-maxdepth", "1"  -- Don't descend into RCS.
    ]

-- | Find files. We use the ASCII NULL terminated paths since file
-- names can contain @\n@ and would get split by @lines@.
mdfind :: Dir -> [String] -> IO [File]
mdfind dir args = fmap (sortOn filename) . mapM parseAbsFile . massage
  -- TODO sort [Note] instead of file paths?
  =<< readProcess "mdfind" (stdArgs dir ++ args) ""
  where
    stdArgs dir' = [ "-onlyin", P.fromAbsDir dir' , "-0" ]
    massage = map unpack
            . filter (not . isSuffixOf "~")   -- Vim backup file.
            . filter (not . isSuffixOf ",v")  -- RCS file.
            . init . splitOn "\0"  -- Null-terminated filenames.
            . pack

findFind :: SearchTool -> Dir -> String -> IO [File]
findFind tool dir s = fmap (sortOn filename) . mapM parseAbsFile . massage
    =<< snd3 <$> readProcessWithExitCode (show tool) (searchOptions tool dir s) ""
                  -- `ack` and `ag` exit with 1 if they find no files so we
                  -- cannot use `readProcess` here.
  where
    snd3 (_, x, _) = x  -- From Neil Mitchell's "extra" package.
    massage = map unpack
            . init . splitOn "\0"  -- Null-terminated filenames.
            . pack

myfilter :: [Char] -> Bool
myfilter file = not ("~"  `L.isSuffixOf` file)  -- Vim backup file.
             && not (",v" `L.isSuffixOf` file)  -- RCS file.

-- | List all files in the directory except for hidden files.
listFiles :: Dir -> IO [File]
listFiles dir =
  sortOn filename . filter (myfilter . fromAbsFile) . snd <$> listDir dir
  -- TODO sort [Note] instead of file paths?

type Dir  = P.Path P.Abs P.Dir
type File = P.Path P.Abs P.File

data Status = Obsoleted | Current deriving (Eq, Ord, Show)
newtype ID = ID LocalTime deriving (Eq, Ord, Show)
type Tag = String
type Name = String
type Contents = Text
type Extension = String
data Note = Note
  { noteStatus :: Status
  , noteID     :: ID
  , noteTag    :: Tag
  , noteName   :: Name
  , noteExt    :: Maybe Extension
  } deriving (Eq, Ord, Show)

notObsolete :: Note -> Bool
notObsolete Note {..} = noteStatus /= Obsoleted

hasTag :: Tag -> Note -> Bool
hasTag t Note {..} = noteTag == t

hasID :: ID -> Note -> Bool
hasID i Note {..} = noteID == i

hasID' :: String -> Note -> Bool
hasID' s n = case parseID s of
  Nothing -> False
  Just i  -> hasID i n

-- | True if Note matches any of the tags.
--
-- >>> hasAnyTag ["a","note"] (Note Current (ltID 1 1 1 1 1) "note" "" Nothing)
-- True
-- >>> hasAnyTag ["a","b"] (Note Current (ltID 1 1 1 1 1) "note" "" Nothing)
-- False
-- >>> hasAnyTag [] (Note Current (ltID 1 1 1 1 1) "note" "" Nothing)
-- False
hasAnyTag :: [Tag] -> Note -> Bool
hasAnyTag tags note = any (`hasTag` note) tags

-- | The format of the ID string.
idFormat :: String
idFormat = "%Y_%m_%d_%H%M"

-- | Parse an ID string.
parseID :: MonadFail m => String -> m ID
parseID = fmap ID . parseTimeM False defaultTimeLocale idFormat

-- | Convert a note ID to its string representation.
  --
  -- >>> formatID (ltID 2019 03 18 10 09)
  -- "2019_03_18_1009"
formatID :: ID -> String
formatID (ID t) = formatTime defaultTimeLocale idFormat t

formatStatus :: Status -> String
formatStatus Obsoleted = "+"
formatStatus _         = ""

-- | Utility function for specifying an ID (mainly for test purposes).
ltID :: Integer -> Int -> Int -> Int -> Int -> ID
ltID y m d h mn = ID $ LocalTime (fromGregorian y m d) (TimeOfDay h mn 0)

-- | Create the filename of a note.
  --
  -- >>> noteFilename (Note Current (ltID 2019 03 18 10 09) "note" "The title" (Just ".txt"))
  -- "2019_03_18_1009-note-The title.txt"
  -- >>> noteFilename (Note Obsoleted (ltID 2019 03 18 10 09) "note" "The title" (Just ".md"))
  -- "+2019_03_18_1009-note-The title.md"
  -- >>> noteFilename (Note Obsoleted (ltID 2019 03 18 10 09) "note" "The title.md" Nothing)
  -- "+2019_03_18_1009-note-The title.md"
noteFilename :: Note -> String
noteFilename = unpack . noteFilenameT

noteFilenameT :: Note -> Text
noteFilenameT Note { .. } = (normalize NFC . pack) $ printf "%s%s-%s-%s%s"
  (formatStatus noteStatus)
  (formatID noteID)
  noteTag
  noteName
  (fromMaybe "" noteExt)

notePath :: Dir -> Note -> FilePath
-- TODO less secure?:
-- notePath dir note = dir System.FilePath.</> noteFilename note
notePath dir = maybe (error "Invalid filename or directory.") fromAbsFile
             . noteAbsFile dir

-- TODO: Consider adding extension in a principled manner with <.>?
noteRelFile :: MonadThrow m => Note -> m (P.Path P.Rel P.File)
noteRelFile = parseRelFile . noteFilename

-- TODO: Consider adding extension in a principled manner with <.>?
noteAbsFile :: MonadThrow m => Dir -> Note -> m File
noteAbsFile dir note = (dir </>) <$> noteRelFile note


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

-- |
--
-- >>> parseTest idP $ pack "2019_02_03_1345"
-- ID 2019-02-03 13:45:00
--
-- >>> parseTest idP $ pack "2019_02_03_1359"
-- ID 2019-02-03 13:59:00
idP :: Parser ID
idP = let sep = char '_' in do
  yyyy <-        count 4 digitChar
  mm   <- sep *> count 2 digitChar
  dd   <- sep *> count 2 digitChar
  hh   <- sep *> count 2 digitChar
  mn   <-        count 2 digitChar
  parseID (printf "%s_%s_%s_%s%s" yyyy mm dd hh mn)

tagP :: Parser String
tagP  = char '-' *> tagnameP <* char '-'  -- TODO åäöÅÄÖ don't work.
tagnameP :: Parser String
tagnameP = some alphaNumChar

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

endP :: Parser ()
endP = eof <|> backupP <|> rcsP

extP :: Parser (Maybe Extension)
extP = optional $ ("." <>) <$> (char '.' *> many alphaNumChar)

-- | Parse a Note from a String (normally a file name)
  -- TODO: Separate extension from title.
  --
  -- >>> parseTest noteParser (pack "+2019_03_18_1009-note-The title.md")
  -- Note {noteStatus = Obsoleted, noteID = ID 2019-03-18 10:09:00, noteTag = "note", noteName = "The title", noteExt = Just ".md"}
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-The title.txt")
  -- Note {noteStatus = Current, noteID = ID 2019-03-18 10:09:00, noteTag = "note", noteName = "The title", noteExt = Just ".txt"}
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-www.klintenas.se.txt")
  -- Note {noteStatus = Current, noteID = ID 2019-03-18 10:09:00, noteTag = "note", noteName = "www.klintenas.se", noteExt = Just ".txt"}
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-www.klintenas.se.txt~")
   -- Note {noteStatus = Current, noteID = ID 2019-03-18 10:09:00, noteTag = "note", noteName = "www.klintenas.se", noteExt = Just ".txt"}
  --
  -- >>> parseTest noteParser (pack "2019_03_18_1009-note-The title")
  -- Note {noteStatus = Current, noteID = ID 2019-03-18 10:09:00, noteTag = "note", noteName = "The title", noteExt = Nothing}
noteParser :: Parser Note
noteParser = Note <$> obsP <*> idP <*> tagP <*> titleP <*> extP <* endP

-- | Extract tags from file names and count the number of uses of each tag.
  -- TODO Use Megaparsec for the extraction to make tag delimiter flexible?
countTags :: [Note] -> [(Int, String)]
countTags = map (length &&& head) . group . sort . map noteTag

-- | Create an ID for a new file based on a given UTCTime. The file ID will be in the
  -- local time corresponding to the UTCTime for the current time zone (which may
  -- not be the time zone in effect at the provided UTC time).
makeIDFromUTCTime :: UTCTime -> IO ID
makeIDFromUTCTime t = ID <$> (flip utcToLocalTime t <$> getCurrentTimeZone)

-- | Create an ID for a new file. Specifically a time stamp
  -- based on the current local time with minute precision.
makeID :: IO ID
makeID = getCurrentTime >>= makeIDFromUTCTime

-- | Provide the next valid ID (one minute later timestamp).
--
-- >>> nextID (ltID 2019 12 31 23 58)
-- ID 2019-12-31 23:59:00
--
-- >>> nextID (ltID 2019 12 31 23 59)
-- ID 2020-01-01 00:00:00
nextID :: ID -> ID
nextID (ID t) = (ID . utcToLocalTime utc . addUTCTime 60 . localTimeToUTC utc) t

-- | Check in notes with RCS. Use default description/message.
checkinNotes :: Dir -> [Note] -> IO ExitCode
checkinNotes dir notes = rawSystem "rcs" (args ++ map (notePath dir) notes)
  where
    args = [ "ci"   -- Check in.
           , "-l"   -- Check out the file locked (with write permissions).
           , "-t-\"Created by nn.\""  -- File description (first checkin).
           , "-m\"Updated by nn.\""   -- Log message (second+ checkins).
           ]

-- | Check in note with RCS. Use default description/message.
checkinNote :: Dir -> Note -> IO ExitCode
checkinNote dir note = checkinNotes dir [note]

-- | Check in file with RCS. The checkin is forced even if the file
-- has not been changed which ensures the log message is written.
checkinForceMessage :: Text -> [File] -> IO ExitCode
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
renameRCS :: Dir -> Note -> Note -> IO ExitCode
renameRCS dir oldNote newNote = do
  old <- noteAbsFile dir oldNote
  new <- noteAbsFile dir newNote
  renameFile old new
  rcsold <- rcsfile old
  rcsnew <- rcsfile new
  renameFile rcsold rcsnew
  checkinForceMessage ("Renamed from \"" <> filename' old <> "\".") [new]
    where
      rcsfile :: File -> IO (File)
      rcsfile file = do
        ext <- fileExtension file
        replaceExtension (ext ++ ",v")
          (parent file </> [reldir|RCS|] </> filename file)

-- | Get the filename (path removed) as @Text@.
  --
  -- The @Text@ is normalized because `mdfind` and @listDir@ does not use NFC
  -- normalisation for file names, so for example
  -- `length "Ö" == 2` in `mdfind` output.
filename' :: P.Path a P.File -> Text
filename' = normalize NFC . pack . fromRelFile . filename

-- | Print the filename (path removed).
printFilename :: Note -> IO ()
printFilename = putStrLn . noteFilename

-- | Show the note
showNote :: Note -> String
showNote Note { .. } = printf "%s%s [%s] %s"
  (formatStatus noteStatus)
  (formatID noteID)
  noteTag
  noteName

-- | Print the note
printNote :: Note -> IO ()
printNote = putStrLn . showNote

-- | Apply a function to a list.
-- If the list is empty @Nothing@ is returned.
-- If the list is non-empty the function is applied.
--
-- >>> safe head []
-- Nothing
-- >>> safe head [1]
-- Just 1
-- >>> safe null []
-- Nothing
-- >>> safe null [1]
-- Just False
safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f xs = Just $ f xs

