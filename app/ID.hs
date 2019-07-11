{-# LANGUAGE LambdaCase #-}

module ID where

import Options (SelectOne (SelectID))
import Select (getOneNote)
import Util (makeID, nextID, Dir, ID, formatID, SearchTool)


firstAvailableID :: SearchTool -> Dir -> ID -> IO ID
firstAvailableID tool dir i = getOneNote tool dir (SelectID $ formatID i) >>= \case
  Nothing -> return i
  _       -> firstAvailableID tool dir (nextID i)

makeAvailableID :: SearchTool -> Dir -> IO ID
makeAvailableID tool dir = makeID >>= firstAvailableID tool dir

