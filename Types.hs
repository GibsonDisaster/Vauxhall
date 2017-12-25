module Types where
  import qualified Data.Map as M

  type Coord = (Int, Int)

  data Item = Sword | Shield | Potion | Coin | Null

  instance Show Item where
    show Sword = "Sword"
    show Shield = "Shield"
    show Potion = "Potion"
    show Coin = "Coin"
    show Null = "Null"

  data Class = Knight {
                kConst :: Int,
                kStr :: Int,
                kDex :: Int,
                kInt :: Int
               }
               |
               Thief {
                tConst :: Int,
                tStr :: Int,
                tDex :: Int,
                tInt :: Int
               }
               |
               Sub {
                sConst :: Int,
                sStr :: Int,
                sDex :: Int,
                sInt :: Int
               }

  instance Show Class where
    show (Knight _ _ _ _) = "Knight"
    show (Thief _ _ _ _) = "Thief"
    show (Sub _ _ _ _) = "Sub"

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
                hExp :: Int,
                hLvl :: Int,
                hClass :: Class,
                items :: [Item]
              } deriving Show

  data Direction = Up | Down | Left | Right deriving Show

  data Action = OpenDoor | CloseDoor | PickUp | DropItem | ShowInv | Idle deriving Show

  data Event = Dir Direction | Exit | PlayerAction Action deriving Show