{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Types where
  import qualified Data.Map as M
  import Control.Lens hiding (getConst)
  import Data.Binary
  import GHC.Generics (Generic)

  type Coord = (Int, Int)

  data Item = Potion | Coin | Null deriving (Eq, Generic)
  
  data Direction = Up | Down | Left | Right | Stay deriving (Show, Generic)

  data Action = OpenDoor | CloseDoor | PickUp | DropItem | Rest | ShowInv | ShowStats | Idle | GoDown | GoUp | Quaff | Inspect | Buy | Kick | Save | CastSpell | Rush | Help | Debug deriving (Show, Generic)

  data Event = Dir Direction | Exit | PlayerAction Action deriving (Show, Generic)
  
  data Effect = Dmg { _eDur :: Int } | Psn { _eDur :: Int } | Slp { _eDur :: Int } | None deriving (Eq, Generic)

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
               } deriving (Eq, Generic)

  data World = World {
                _mode :: String,
                _wHero :: Hero,
                _walls :: [String],
                _currentLvl :: String,
                _tileMap :: M.Map Coord Char,
                _wItems :: M.Map Coord Item,
                _wEnemies :: M.Map String [Enemy],
                _currEnemies :: [Enemy],
                _wStairs :: M.Map String [Staircase],
                _wInspects :: M.Map (Coord, String) [String],
                _wShops :: M.Map (Coord, String) (Item, Int),
                _wTraps :: M.Map String [Trap],
                _wCurrFounts :: [Fountain]
               } deriving (Show, Generic)

  data Enemy = Enemy {
                _eCoord :: Coord,
                _eOldCoord :: Coord,
                _eHealth :: Int
               } deriving (Show, Eq, Generic)

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
                _hMoney :: Int,
                _hEffects :: [Effect],
                _hSpells :: [Spell]
              } deriving (Show, Generic)

  data Staircase = Staircase {
                    _sDir :: Char,
                    _sDest :: String,
                    _sCoord :: Coord
                   } deriving (Show, Generic)

  data Trap = Trap {
                _tEffect :: Effect,
                _tCoord :: Coord,
                _tDuration :: Int
              } deriving (Show, Generic)

  data Spell = Spell {
                _spCoord :: Coord,
                _spEffect :: Effect,
                _spDir :: Direction
               } deriving (Show, Generic)

               
  data Fountain = Fountain {
                    _fCoord :: Coord,
                    _fEffect :: Effect,
                    _fQuaffed :: Bool
                  } deriving (Show, Generic)

  {- Will need to implement custom Read class as well to use these -}

  instance Show Class where
    show (Knight _ _ _ _) = "Knight"
    show (Thief _ _ _ _) = "Thief"
    show (Sub _ _ _ _) = "Sub"
    show (PolkaKing _ _ _ _) = "Polka King"

  instance Show Item where
    show Potion = "Potion"
    show Coin = "Coin"
    show Null = "Null"

  instance Show Effect where
    show None = ""
    show (Dmg d) = "Dmg " ++ (show d) ++ "t"
    show (Psn d) = "Psn " ++ (show d) ++ "t"

  instance Binary Item
  instance Binary Direction
  instance Binary Action
  instance Binary Event
  instance Binary Effect
  instance Binary Class
  instance Binary World
  instance Binary Enemy
  instance Binary Hero
  instance Binary Staircase
  instance Binary Trap
  instance Binary Spell
  instance Binary Fountain

  makeLenses ''World
  makeLenses ''Enemy
  makeLenses ''Hero
  makeLenses ''Staircase
  makeLenses ''Trap