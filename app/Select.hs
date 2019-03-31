{-# LANGUAGE RecordWildCards #-}

module Select where

import Control.Monad (join, sequence)
import Data.Either (isLeft, isRight, fromRight, rights, lefts)
import Path (Path (..), Abs (..), Rel (..), File (..))
import Text.Megaparsec (parse)

import NNUtil
import Options

getOneNote :: Dir -> SelectOne -> IO [Note]
getOneNote dir  SelectLast  = getLastNote dir
getOneNote dir (SelectID i) = getIDNote dir i

getManyNotes :: Dir -> SelectMulti -> IO [Note]
getManyNotes dir Multi {..} = join <$> sequence
  ( map (getIDNote dir) sIDs
--  ++ if sLast then pure <$> getLastNote dir else pure []
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
-- TODO: Should return Maybe Note? (May fail if empty note DB.
getLastNote :: Dir -> IO [Note]
getLastNote dir = (\l -> if null l then [] else [last l]) <$> getNotes dir [] []

-- TODO getNote should return Maybe Note? (What if two files have same ID?)
getIDNote :: Dir -> String -> IO [Note]
getIDNote dir id = processNotes f <$> mdfind dir ["name:"++id]
  where
    f = hasID (ID $ splitParts id)  -- TODO maybe check that the provided ID is valid??
