module Types where
  import qualified Data.Map as M

  type Coord = (Int, Int)

  data World = World {
                wHero :: Hero,
                walls :: [String],
                tileMap :: M.Map Coord Char
               } deriving Show

  data Hero = Hero {
                hCoord :: Coord,
                hOldCoord :: Coord
              } deriving Show

  data State = State {
                currentAction :: Event
               }

  data Direction = Up | Down | Left | Right deriving Show

  data Action = OpenDoor | CloseDoor | Idle deriving Show

  data Event = Dir Direction | Exit | PlayerAction Action deriving Show