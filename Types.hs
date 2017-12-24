module Types where
  import qualified Data.Map as M

  type Coord = (Int, Int)

  data Item = Sword | Shield | Potion | Null

  instance Show Item where
    show Sword = "Sword"
    show Shield = "Shield"
    show Potion = "Potion"
    show Null = "Null"

  data World = World {
                wHero :: Hero,
                walls :: [String],
                tileMap :: M.Map Coord Char,
                wEnemies :: [Enemy]
               } deriving Show

  data Enemy = Enemy {
                eCoord :: Coord,
                eOldCoord :: Coord,
                eHealth :: Int
               } deriving Show

  data Hero = Hero {
                hCoord :: Coord,
                hOldCoord :: Coord,
                hHealth :: Int,
                items :: [Item]
              } deriving Show

  data Direction = Up | Down | Left | Right deriving Show

  data Action = OpenDoor | CloseDoor | PickUp | DropItem | ShowInv | Idle deriving Show

  data Event = Dir Direction | Exit | PlayerAction Action deriving Show