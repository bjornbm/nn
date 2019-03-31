{-# LANGUAGE RecordWildCards #-}

module Select where

import Control.Monad (join, sequence)
import Data.Either (isLeft, isRight, fromRight, rights, lefts)
import Data.List (sort)
import Data.Maybe (catMaybes, maybeToList)
import Path (Path (..), Abs (..), Rel (..), File (..))
import Text.Megaparsec (parse)

import NNUtil
import Options


-- TODO: Last should work with TERMs and TAGs as for the SelectMany case!
getOneNote :: Dir -> SelectOne -> IO (Maybe Note)
getOneNote dir  SelectLast  = getLastNote dir
getOneNote dir (SelectID i) = getIDNote dir i

-- | Get many notes based on various selection criteria.
--
-- --id always selects the first note with the given ID, regardless of other flags.
-- if any other selection options are specified all files matching TERMS
-- are selected, or all files in the DB if no TERMS were specified.
-- Then the selected files are narrowed down by TAGs.
-- If --last is specified only the last file of the narrowed down list is
-- selected and all others are discarded.
getManyNotes :: Dir -> SelectMany -> IO [Note]
getManyNotes dir SelectMany {..} = do
  if not sLast && null sIDs && null sTAGs && null sTERMs
    then  -- No options specified, default to listing all notes
      getAllNotes dir
    else do
      nis <- catMaybes <$> mapM (getIDNote dir) sIDs
      if not sLast && null sTAGs && null sTERMs
        then return nis  -- only IDs were specified.
        else do
          ns <- if null sTERMs
                  then getAllNotes dir  -- Default to matching notes
                  else getMDNotes dir sTERMs
          -- Filter by tags
          let xs = if null sTAGs then ns else filter (hasAnyTag sTAGs) ns
          let ys = if sLast then maybeToList $ safe last xs else xs
          return . sort $ nis ++ ys
  -- TODO

getTagNotes :: Dir -> [Tag] -> IO [Note]
getTagNotes dir tags = processNotes (f tags) <$> mdlist dir
  where                            -- TODO respect sJoin status?!
    f tags note = notObsolete note && any (flip hasTag note) tags

getNotes :: Dir -> [Tag] -> [String] -> IO [Note]
getNotes dir []   []    = processNotes notObsolete <$> mdlist dir
getNotes dir []   terms = processNotes notObsolete <$> mdfind dir terms
-- TODO is this slow compared to using mdfind with tags? Seems fast enough?
getNotes dir tags []    = processNotes (f tags)    <$> mdlist dir
  where                            -- TODO respect sJoin status!
    f tags note = notObsolete note && any (flip hasTag note) tags
-- TODO Multiple tags broken; since used as search terms to mdfind they
-- are somehow ANDed (i.e., not must have one of the tags, but also in
-- some way match all other tags in contents or otherwise).
getNotes dir tags terms = processNotes (f tags)    <$> mdfind dir terms --(tags <> terms)
  where                            -- TODO respect sJoin status!
    f tags note = notObsolete note && any (flip hasTag note) tags

processNotes :: (Note -> Bool) -> [Path Abs File] -> [Note]
processNotes f = filter f . rights . map (parse noteParser "" . filename')

-- | Get the note with most recent timestamp.
-- Returns @Nothing@ if the note database is empty.
-- If two notes share the latest timestamp only one of them will be returned.
getLastNote :: Dir -> IO (Maybe Note)
getLastNote dir = safe last <$> getAllNotes dir

-- | Get the note with the given ID.
-- Returns @Nothing@ if no note matches the ID.
-- If two notes have the same ID only one of them will be returned.
getIDNote :: Dir -> String -> IO (Maybe Note)
getIDNote dir id = safe head . processNotes f <$> mdfind dir ["name:"++id]
  where
    f = hasID (ID $ splitParts id)  -- TODO maybe check that the provided ID is valid??

-- | Get all notes in the DB.
getAllNotes :: Dir -> IO [Note]
getAllNotes dir = processNotes notObsolete <$> mdlist dir  -- TODO allow obsolete

-- | Get all notes which match the metadata terms
getMDNotes :: Dir -> [String] -> IO [Note]
getMDNotes dir terms = processNotes notObsolete <$> mdfind dir terms
