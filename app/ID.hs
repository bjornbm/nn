{-# LANGUAGE LambdaCase #-}

module ID where

import Options (SelectOne (SelectID))
import Select (getOneNote)
import Util (makeID, nextID, Dir, ID, formatID)


firstAvailableID :: Dir -> ID -> IO ID
firstAvailableID dir i = getOneNote dir (SelectID $ formatID i) >>= \case
  Nothing -> return i
  _       -> firstAvailableID dir (nextID i)

makeAvailableID :: Dir -> IO ID
makeAvailableID dir = makeID >>= firstAvailableID dir

