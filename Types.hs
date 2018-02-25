module Types where
  import qualified Data.Map as M

  type Coord = (Int, Int)

  data Item = Potion | Coin | Null deriving Eq

  instance Show Item where
    show Potion = "Potion"
    show Coin = "Coin"
    show Null = "Null"

  data Class = Knight {
                _kConst :: Int,
                _kStr :: Int,
                _kDex :: Int,
                _kInt :: Int
               }
               |
               Thief {
                _tConst :: Int,
                _tStr :: Int,
                _tDex :: Int,
                _tInt :: Int
               }
               |
               Sub {
                _sConst :: Int,
                _sStr :: Int,
                _sDex :: Int,
                _sInt :: Int
               }
               |
               PolkaKing {
                 _pConst :: Int,
                 _pStr :: Int,
                 _pDex :: Int,
                 _pInt :: Int
               } deriving Eq

  instance Show Class where
    show (Knight _ _ _ _) = "Knight"
    show (Thief _ _ _ _) = "Thief"
    show (Sub _ _ _ _) = "Sub"
    show (PolkaKing _ _ _ _) = "Polka King"

  data World = World {
                _wHero :: Hero,
                _walls :: [String],
                _currentLvl :: String,
                _tileMap :: M.Map Coord Char,
                _wItems :: M.Map Coord Item,
                _wEnemies :: M.Map String [Enemy],
                _currEnemies :: [Enemy],
                _wInspects :: M.Map (Coord, String) [String],
                _wShops :: M.Map (Coord, String) (Item, Int)
               } deriving Show

  data Enemy = Enemy {
                _eCoord :: Coord,
                _eOldCoord :: Coord,
                _eHealth :: Int
               } deriving (Show, Eq)

  data Hero = Hero {
                _hName :: String,
                _hCoord :: Coord,
                _hOldCoord :: Coord,
                _hHealth :: Int,
                _hDmg :: Int,
                _hExp :: Int,
                _hLvl :: Int,
                _hClass :: Class,
                _items :: [Item],
                _hScore :: Int,
                _hMoney :: Int
              } deriving Show

  data Direction = Up | Down | Left | Right | Stay deriving Show

  data Action = OpenDoor | CloseDoor | PickUp | DropItem | Rest | ShowInv | ShowStats | Idle | GoDown | GoUp | Quaff | Inspect | Buy deriving Show

  data Event = Dir Direction | Exit | PlayerAction Action deriving Show