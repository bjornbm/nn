{-# LANGUAGE RecordWildCards #-}

module Select where

import Control.Monad (join, sequence)
import Data.Either (isLeft, isRight, fromRight, rights, lefts)
import Data.Maybe (catMaybes)
import Path (Path (..), Abs (..), Rel (..), File (..))
import Text.Megaparsec (parse)

import NNUtil
import Options

getOneNote :: Dir -> SelectOne -> IO (Maybe Note)
getOneNote dir  SelectLast  = getLastNote dir
getOneNote dir (SelectID i) = getIDNote dir i


getManyNotes :: Dir -> SelectMulti -> IO [Note]
getManyNotes dir Multi {..} = catMaybes <$> sequence
  ( map (getIDNote dir) sIDs
  ++ [if sLast then getLastNote dir else return Nothing]
--  ++ map (getNotes sTAGs sTERMs)
  )



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
getLastNote dir = safe last <$> getNotes dir [] []


-- | Get the note with the given ID.
-- Returns @Nothing@ if no note matches the ID.
-- If two notes have the same ID only one of them will be returned.
getIDNote :: Dir -> String -> IO (Maybe Note)
getIDNote dir id = safe head . processNotes f <$> mdfind dir ["name:"++id]
  where
    f = hasID (ID $ splitParts id)  -- TODO maybe check that the provided ID is valid??
