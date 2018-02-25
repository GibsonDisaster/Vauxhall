{-# LANGUAGE TemplateHaskell #-}

import Lens.Simple

data Player = Player {
  _x :: Int,
  _y :: Int,
  _alive :: Bool
}

data World = World {
  __player :: Player
}

makeLenses ''Player
makeLenses ''World