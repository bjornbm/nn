{-# LANGUAGE RecordWildCards #-}

module Select (getOneNote, getManyNotes, getAllNotes, getMDNotes, getBadFiles) where

import Data.Either (isLeft, rights)
import Data.List (sort)
import Data.Maybe (catMaybes, maybeToList)
import Text.Megaparsec (parse)

import Util
import Options


-- TODO: Last should work with TERMs and TAGs as for the SelectMany case!
getOneNote :: SearchTool -> Dir -> SelectOne -> IO (Maybe Note)
getOneNote _    dir  SelectLast  = getLastNote dir
getOneNote tool dir (SelectID i) = getIDNote tool dir i

-- | Get many notes based on various selection criteria.
--
-- --id always selects the first note with the given ID, regardless of other flags.
-- if any other selection options are specified all files matching TERMS
-- are selected, or all files in the DB if no TERMS were specified.
-- Then the selected files are narrowed down by TAGs.
-- If --last is specified only the last file of the narrowed down list is
-- selected and all others are discarded.
getManyNotes :: SearchTool -> Dir -> SelectMany -> IO [Note]
getManyNotes tool dir SelectMany {..} =
  if not selectManyLast && null selectManyIDs && null selectManyTags && null selectManyTerms
    then  -- No options specified, default to listing all notes
      getAllNotes dir
    else do
      nis <- catMaybes <$> mapM (getIDNote tool dir) selectManyIDs
      if not selectManyLast && null selectManyTags && null selectManyTerms
        then return nis  -- only IDs were specified.
        else do
          ns <- if null selectManyTerms
                  then getAllNotes dir  -- Default to matching notes
                  else getMDNotes dir selectManyTerms
          -- Filter by tags
          let xs = if null selectManyTags
                      then ns
                      else filter (hasAnyTag selectManyTags) ns
          let ys = if selectManyLast then maybeToList $ safe last xs else xs
          return . sort $ nis ++ ys


parseNotes :: [File] -> [Note]
parseNotes = rights . map (parse noteParser "" . filename')

-- | Get the note with most recent timestamp.
-- Returns @Nothing@ if the note database is empty.
-- If two notes share the latest timestamp only one of them will be returned.
getLastNote :: Dir -> IO (Maybe Note)
getLastNote dir = safe last <$> getAllNotes dir

-- | Get the note with the given ID.
-- Returns @Nothing@ if no note matches the ID.
-- If two notes have the same ID only one of them will be returned.
getIDNote :: SearchTool -> Dir -> String -> IO (Maybe Note)
getIDNote tool dir i = safe head . filter f . parseNotes <$> findFind tool dir i
  where
    f = hasID' i  -- TODO maybe check that the provided ID is valid??

-- | Get all notes in the DB.
getAllNotes :: Dir -> IO [Note]
getAllNotes dir = filter notObsolete . parseNotes <$> listFiles dir  -- TODO allow obsolete

getBadFiles :: Dir -> IO [File]
getBadFiles dir = filter (isLeft . parse noteParser "" . filename') <$> listFiles dir

-- | Get all notes which match the metadata terms
getMDNotes :: Dir -> [String] -> IO [Note]
getMDNotes dir terms = filter notObsolete . parseNotes <$> mdfind dir terms
