{-# LANGUAGE TemplateHaskell #-}

module Main where
  import Prelude hiding (Either(..))
  import Data.List (delete)
  import Control.Lens hiding (getConst)
  import System.Console.ANSI
  import System.IO
  import System.Random
  import System.Environment (getArgs)
  import qualified Data.Map.Strict as M
  import Types

  {- Written By Henning Tonko ☭ -}

  {-
  TODO:
    [X] Add money
    [X] Keep enemy within impassible objects (Check coords and make sure they are both correct)
    [X] Implement Enemy attacking
    [X] Implement Player attacking
    [X] Implement killing player/enemy when health <= 0
    [X] Stop Enemy from walking into player
    [X] Leveling/Classes/Attributes
    [X] Title Screen
    [X] Menu where you pick your name and class at beginning
    [X] Way to check player status (To right side of map)
    [X] Up/Down Staircases
    [X] Specific enemies for each floor!!!!!
    [X] Dialogue spots on floors (marked with a '?')
    [] Clean up code!!!!
    [] Make attributes matter
    [X] Add a shop or a use for money
    [X] Random item placement
    [X] Add health potions
    [X] Remove gear from the ground
    [?] Multiple kinds of Enemies
    [] Make a better title screen
    [] Make it FUN!!!!
    [] Boss at the end called "The Relayer"
    [] Branching paths
    [X] Change wallsList to just accept a [(String, [String])] and then put it all together
    [X] Score kept throughout game 
    [X] Score displayed at end/death
        - Killing enemies [X]
        - Picking up items [X]
        - Picking up gold [X]
  -}

  titleStrings :: [String]
  titleStrings =  [ "                                        ",
                    "                                        ",
                    " __      __              _           _ _", 
                    " \\ \\    / /             | |         | | |",
                    "  \\ \\  / /_ _ _   ___  _| |__   __ _| | |",
                    "   \\ \\/ / _` | | | \\ \\/ / '_ \\ / _` | | |",
                    "    \\  / (_| | |_| |>  <| | | | (_| | | |",
                    "     \\/ \\__,_|\\__,_/_/\\_\\_| |_|\\__,_|_|_|",
                    "                                        ",
                    "               By Henning Tonko             ",
                    "                                        ",
                    "              <press any button>        "
                  ]

  wall1 :: [String]
  wall1 = ["-----------------------     ----------",
           "|<                    |     |    >   |",
           "|                     ---|---        |",
           "|                        +           |",
           "|                     ---|---        |",
           "|                     |     |        |",
           "-----------------------     ----------"]

  wall2 :: [String]
  wall2 = ["--------------------------------------",
           "|>                    |     |    <   |",
           "|                     ---+---        |",
           "|                                    |",
           "|                                    |",
           "|                                    |",
           "--------------------------------------"]

  wall3 :: [String]
  wall3 = ["--------------------------------------",
           "|<            ~~~~~                  |",
           "|               ~~~~                 |",
           "|                ~~~~              > |",
           "|                 ~~~~~              |",
           "|                 ~~~~~~~~           |",
           "--------------------------------------"]

  wall4 :: [String]
  wall4 = ["-------------------------------------   ----------",
          "|                                    |   |$ SHOP $|",
          "|                  |                 |---|--------|",
          "|                  |               <   +    ppp   |",
          "|                $-+-$               |---| p  p   |",
          "|>                 |                 |   |  p     |",
          "--------------------------------------   ----------"]

  wall5 :: [String]
  wall5 = ["--------------------------------------",
          "|         I         I                |",
          "|         IIIII+IIIII                |",
          "|                                  > |",
          "|              ?                     |",
          "|<                                   |",
          "--------------------------------------"]

  spawnEnemy :: Coord -> Int -> Enemy
  spawnEnemy c h = Enemy { _eCoord = c, _eOldCoord = (0, 0), _eHealth = h }

  wallsList :: M.Map String [String]
  wallsList = M.fromList [
                          ("wall1", wall1),
                          ("wall2", wall2),
                          ("wall3", wall3),
                          ("wall4", wall4),
                          ("wall5", wall5)
                         ]

  enemiesList :: M.Map String [Enemy]
  enemiesList = M.fromList [
                            ("wall1", [spawnEnemy (33, 3) 6]),
                            ("wall2", [spawnEnemy (4, 4) 7]),
                            ("wall3", [spawnEnemy (8, 2) 6, spawnEnemy (33, 5) 5]),
                            ("wall4", [spawnEnemy (1, 1) 9]),
                            ("wall5", [spawnEnemy (14, 1) 90])
                           ]

  inspectList :: M.Map (Coord, String) [String]
  inspectList = M.fromList [ 
                            (((15,4), "wall5"), ["Hello My name is Jan Lawen!", "I am the disgraced polka king of Pennsylvania", "Wouldn't you like to be me?"])
                           ]

  shopList :: M.Map (Coord, String) (Item, Int)
  shopList = M.fromList [
                         (((45, 3), "wall4"), (Potion, 10)),
                         (((44, 3), "wall4"), (Potion, 10)),
                         (((46, 3), "wall4"), (Potion, 10)),
                         (((43, 4), "wall4"), (Potion, 10)),
                         (((45, 5), "wall4"), (Potion, 10)),
                         (((46, 4), "wall4"), (Potion, 10)),
                         (((44, 5), "wall4"), (Potion, 10))
                        ]
                        
  {- Lens Functions -}

  getHeroName :: World -> String
  getHeroName = view (wHero . hName)

  {-
  Constants that must be used when determining a position
  because of ANSI's insane coordinate system.
  (I think this might actually be my fault)
  -}

  rConst :: Coord
  rConst = (1, -1)

  lConst :: Coord
  lConst = (-1, 1)

  uConst :: Coord
  uConst = (1, -1)

  dConst :: Coord
  dConst = (-1, 1)

  {- Player Class List -}

  knight :: Class
  knight = Knight { _kConst = 15, _kStr = 4, _kDex = 7, _kInt = 3 }

  thief :: Class
  thief = Thief { _tConst = 8, _tStr = 3, _tDex = 10, _tInt = 6 }

  sub :: Class
  sub = Sub { _sConst = 10, _sStr = 2, _sDex = 15, _sInt = 1 }

  polkaKing :: Class
  polkaKing = PolkaKing { _pConst = 17, _pStr = 2, _pDex = 11, _pInt = 900 }

  debug :: String -> IO ()
  debug s = appendFile "test.txt" (s ++ "\n")

  flipCoord :: Coord -> Coord
  flipCoord (x, y) = (y, x)

  getItem :: Char -> Item
  getItem 'p' = Potion
  getItem '$' = Coin
  getItem _ = Null

  getVect :: Direction -> Coord
  getVect Up = (-1, 0) |+| uConst
  getVect Down = (1, 0) |+| dConst
  getVect Left = (0, -1) |+| lConst
  getVect Right = (0, 1) |+| rConst
  getVect Stay = (0, 0)

  {- Check to see if a given coord on the level is impassible (can't walk there) -}

  isImpassible :: M.Map Coord Char -> Coord -> Bool
  isImpassible m coord = case M.lookup coord m of
                          Nothing -> True
                          Just c -> case c of
                                     '-' -> True
                                     '|' -> True
                                     '+' -> True
                                     'I' -> True
                                     _ -> False

  isAShopItem :: World -> Coord -> String -> Bool
  isAShopItem w c l = case M.lookup (c, l) (_wShops w) of
                       Nothing -> False
                       Just t -> True

  isOkToPlace :: M.Map Coord Char -> [(Coord, Char)]
  isOkToPlace m = filter (\(_, ch) -> ch == ' ') m'
    where
      m' = M.toList m

  {- Spawn items in any blank tiles -}
  
  spawnItems :: M.Map Coord Char -> Int -> IO [(Coord, Item)]
  spawnItems m n = do
    let okSpots = isOkToPlace m
    placeOfItems <- mapM (\_ -> randChoice okSpots) [1..n] -- [(Coord, Char)]
    i <- randChoice [Coin, Coin, Potion]
    let spawnedItems = map (\((x, y), _) -> ((x, y), i)) placeOfItems -- [Coord, Item]
    return spawnedItems

  findEnemyByCoord :: [Enemy] -> Coord -> Maybe Enemy
  findEnemyByCoord es c
    | length es == 0 = Nothing
    | length es == 1 = if (_eCoord (head es)) == (_eCoord dummy) then Just (head es) else Nothing
    | (_eCoord (head es)) == (_eCoord dummy) = Just (head es)
    | otherwise = findEnemyByCoord (tail es) c
      where
        dummy = Enemy { _eCoord = c, _eOldCoord = (1, 1), _eHealth = 10 }

  getNextLvl :: String -> String
  getNextLvl lvl = "wall" ++ show nextNum
    where
      nextNum = (read (drop 4 lvl) :: Int) + 1

  getLastLvl :: String -> String
  getLastLvl lvl = "wall" ++ show lastNum
    where
      lastNum = (read (drop 4 lvl) :: Int) - 1

  {- Next 3 functions are used to create a coordinate map to chars from a given list of strings -}

  mapWalls :: [String] -> M.Map Coord Char
  mapWalls walls = foldl M.union M.empty $ mapWorld walls 0

  mapLevel :: M.Map Coord Char -> Int -> Int -> String -> M.Map Coord Char
  mapLevel m x y str
    | length str == 1 = M.insert (x, y) (head str) m
    | otherwise = M.insert (x, y) (head str) (mapLevel m (x+1) y (tail str))

  mapWorld :: [String] -> Int -> [M.Map Coord Char]
  mapWorld strs lvl
    | length strs == 1 = [mapLevel M.empty 0 lvl (head strs)]
    | otherwise = [mapLevel M.empty 0 lvl (head strs)] ++ mapWorld (tail strs) (lvl + 1)

  {- Coord manipulation -}

  (|+|) :: Coord -> Coord -> Coord
  (|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  (|-|) :: Coord -> Coord -> Coord
  (|-|) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

  dropQuotes :: String -> String
  dropQuotes str = (init . drop 1) str

  {- Getters for class attributes -}

  getConst :: Class -> Int
  getConst c
    | c == knight = _kConst c
    | c == thief = _tConst c
    | c == sub = _sConst c
    | c == polkaKing = _pConst c
    
  getStr :: Class -> Int
  getStr c
    | c == knight = _kStr c
    | c == thief = _tStr c
    | c == sub = _sStr c
    | c == polkaKing = _pStr c

  getDex :: Class -> Int
  getDex c
    | c == knight = _kDex c
    | c == thief = _tDex c
    | c == sub = _sDex c
    | c == polkaKing = _pDex c

  getInt :: Class -> Int
  getInt c
    | c == knight = _kInt c
    | c == thief = _tInt c
    | c == sub = _sInt c
    | c == polkaKing = _pInt c

  {- Look up char in map and check if tile matches given char -}
  isThatChar :: Coord -> Char -> M.Map Coord Char -> Bool
  isThatChar c ch m = case M.lookup c m of
                       Nothing -> False
                       Just ch' -> ch == ch'

  changeTile :: Coord -> Char -> Char -> M.Map Coord Char -> M.Map Coord Char
  changeTile coord c1 c2 m
    | isThatChar coord c1 m = r
    | otherwise = m
    where
      d = M.delete coord m
      r = M.insert coord c2 d

  randChoice :: [a] -> IO a
  randChoice l = do
    n <- (randomRIO (0, ((length l) - 1)))
    return (l !! n)

  moveEnemy :: World -> Enemy -> IO Enemy
  moveEnemy w e = do
    dir <- randChoice [Up, Down, Left, Right, Stay, Stay, Stay]
    let newCoord = (flipCoord (getVect dir) |+| (_eCoord e))
    return e { _eCoord = if (isImpassible (_tileMap w) (newCoord)) then (_eCoord e) else newCoord, _eOldCoord = (_eOldCoord e) }

  moveEnemies :: World -> [Enemy] -> IO [Enemy]
  moveEnemies w es = mapM (moveEnemy w) es

  updateEnemies :: [Enemy] -> Enemy -> Coord -> [Enemy]
  updateEnemies es e c = let foundE = case findEnemyByCoord es (_eCoord e) of
                                       Just e -> e
                                       Nothing -> undefined
                             removeE = filter (\en -> (_eHealth en > 0)) (filter (/= foundE) es)
                             newE = removeE ++ [e { _eHealth = (_eHealth e) - 1 }] in newE

  deadEnemies :: [Enemy] -> [Enemy]
  deadEnemies es
    | length es == 0 = []
    | length es == 1 = if (_eHealth (head es)) <= 0 then [] else [(head es)]
    | otherwise = (if (_eHealth (head es)) <= 0 then [] else [(head es)]) ++ deadEnemies (tail es)

  checkLevel :: Hero -> Hero
  checkLevel h
    | (_hExp h) >= 20 = h { _hExp = 0, _hLvl = (_hLvl h) + 1 }
    | otherwise = h

  getInput :: IO Event
  getInput = do
    char <- getChar
    case char of
      'q' -> return Exit
      'w' -> return (Dir Up)
      's' -> return (Dir Down)
      'a' -> return (Dir Left)
      'd' -> return (Dir Right)
      'k' -> return (Dir Up)
      'j' -> return (Dir Down)
      'h' -> return (Dir Left)
      'l' -> return (Dir Right)
      'o' -> return (PlayerAction OpenDoor)
      'c' -> return (PlayerAction CloseDoor)
      ',' -> return (PlayerAction PickUp)
      'i' -> return (PlayerAction ShowInv)
      'f' -> return (PlayerAction ShowStats)
      '>' -> return (PlayerAction GoDown)
      '<' -> return (PlayerAction GoUp)
      'r' -> return (PlayerAction Rest)
      'z' -> return (PlayerAction Quaff)
      ' ' -> return (PlayerAction Inspect)
      'b' -> return (PlayerAction Buy)
      _  -> getInput

  handleExit :: World -> IO ()
  handleExit w = do
    clearScreen
    setCursorPosition 0 0
    showCursor
    setSGR [Reset]
    putStrLn "Thanks for playing!"
    putStrLn " ___________"
    putStrLn "/           \\"
    putStrLn "|   R.I.P   |"
    putStrLn "|           |"
    putStrLn "|           |"
    putStrLn "|           |"
    putStrLn "|           |"
    putStrLn "|     |     |"
    putStrLn "|   --*--   |"
    putStrLn "|     |     |"
    putStrLn "|     |     |"
    putStrLn "-------------"
    setCursorPosition 5 3
    putStrLn (dropQuotes (show $ getHeroName w))
    setCursorPosition 13 0
    putStrLn ("Your Score was: " ++ (show $ _hScore (_wHero w)))
    showCursor
    setSGR [Reset]

  handleEvent :: World -> Event -> IO ()
  handleEvent w (Dir d) = do
    ems <- moveEnemies w (_currEnemies w)
    let eFighting = findEnemyByCoord (_currEnemies w) (flipCoord newHCoord)
    case eFighting of
     Just e -> gameLoop w { _wHero = (_wHero w) { _hHealth = (_hHealth (_wHero w)) - 1, _hExp = (_hExp (_wHero w)) + 2, _hScore = (_hScore (_wHero w)) + 2 }, _currEnemies = (updateEnemies (_currEnemies w) e newHCoord)}
     Nothing -> gameLoop w { _wHero = if (isImpassible (_tileMap w) (flipCoord newHCoord)) then (_wHero w) else newH, _currEnemies = ems }
    where
      oldH = _hCoord (_wHero w)
      newHCoord = case d of
                   Left -> (0, -1) |+| oldH
                   Right -> (0, 1) |+| oldH
                   Up -> (-1, 0) |+| oldH
                   Down -> (1, 0) |+| oldH
      newH = (_wHero w) { _hCoord = newHCoord, _hOldCoord = oldH, _hHealth = _hHealth (_wHero w), _items = _items (_wHero w)}
  handleEvent w (PlayerAction OpenDoor) = do
    dir <- getInput
    let t = case dir of
             Dir Right -> (0, 1) |+| (flipCoord (_hCoord (_wHero w)) |+| rConst)
             Dir Left -> (0, -1) |+| (flipCoord (_hCoord (_wHero w)) |+| lConst)
             Dir Up -> (-1, 0) |+| (flipCoord (_hCoord (_wHero w)) |+| uConst)
             Dir Down -> (1, 0) |+| (flipCoord (_hCoord (_wHero w)) |+| dConst)
             _ -> (_hCoord (_wHero w))
    gameLoop w { _tileMap = changeTile t '+' 'o' (_tileMap w)}
  handleEvent w (PlayerAction CloseDoor) = do
    dir <- getInput
    let t = case dir of
             Dir Right -> (0, 1) |+| (flipCoord (_hCoord (_wHero w)) |+| rConst)
             Dir Left -> (0, -1) |+| (flipCoord (_hCoord (_wHero w)) |+| lConst)
             Dir Up -> (-1, 0) |+| (flipCoord (_hCoord (_wHero w)) |+| uConst)
             Dir Down -> (1, 0) |+| (flipCoord (_hCoord (_wHero w)) |+| dConst)
             _ -> (_hCoord (_wHero w))
    gameLoop w { _tileMap = changeTile t 'o' '+' (_tileMap w) }
  handleEvent w (PlayerAction PickUp) = do
    let i = case M.lookup (flipCoord (_hCoord (_wHero w))) (_wItems w) of
             Nothing -> Null
             Just c -> c
    let newHero = (_wHero w) { _items = (_items (_wHero w)) ++ [i], _hScore = (_hScore (_wHero w)) + (if i == Coin then 10 else 1), _hMoney = (_hMoney (_wHero w)) + (if i == Coin then (5) else 0)}
    let oldHero = (_wHero w)
    if (isAShopItem w (flipCoord (_hCoord (_wHero w))) (_currentLvl w)) then gameLoop w else gameLoop w { _wItems = M.delete (flipCoord (_hCoord (_wHero w))) (_wItems w), _wHero = if i == Null then oldHero else newHero }    
  handleEvent w (PlayerAction ShowInv) = do
    setCursorPosition 0 0
    putStrLn "Inventory"
    putStrLn "========="
    mapM_ putStrLn (map show $ filter (/= Coin) (_items (_wHero w)))
    _ <- getInput
    setCursorPosition 0 0
    mapM_ (\_ -> putStrLn "               ") (map show $ _items (_wHero w))
    putStrLn "                 "
    putStrLn "                 "
    gameLoop w
  handleEvent w (PlayerAction ShowStats) = do
    drawStats (_wHero w)
    _ <- getInput
    clearScreen
    gameLoop w
  handleEvent w (PlayerAction GoDown) = do
    let t = case M.lookup (flipCoord (_hCoord (_wHero w))) (_tileMap w) of
             Nothing -> ' '
             Just c -> c
    let nextStr = getNextLvl (_currentLvl w)
    let nextWalls = case M.lookup nextStr wallsList of
                     Nothing -> (_walls w)
                     Just ch -> ch
    let nextEnemies = case M.lookup nextStr (_wEnemies w) of
                       Nothing -> (_currEnemies w)
                       Just es -> es
    num <- randomRIO (1, 5)
    is <- spawnItems (mapWalls nextWalls) num
    let w' = if t == '>' then w { _walls = (nextWalls), _currentLvl = nextStr, _tileMap = mapWalls nextWalls, _currEnemies = nextEnemies, _wEnemies = M.insert (_currentLvl w) (_currEnemies w) (_wEnemies w) } else w
    let w'' = w' { _wItems = M.fromList is }
    gameLoop w'
  handleEvent w (PlayerAction GoUp) = do
    let t = case M.lookup (flipCoord (_hCoord (_wHero w))) (_tileMap w) of
              Nothing -> ' '
              Just c -> c
    let nextStr = getLastLvl (_currentLvl w)
    let lastWalls = case M.lookup nextStr wallsList of
                      Nothing -> (_walls w)
                      Just ch -> ch
    let lastEnemies = case M.lookup nextStr (_wEnemies w) of
                       Nothing -> (_currEnemies w)
                       Just es -> es
    is <- spawnItems (mapWalls lastWalls) 5
    let w' = if t == '<' then w { _walls = (lastWalls), _currentLvl = nextStr, _tileMap = mapWalls lastWalls, _currEnemies = lastEnemies, _wEnemies = M.insert (_currentLvl w) (_currEnemies w) (_wEnemies w) } else w
    let w'' = w' { _wItems = M.fromList is }
    gameLoop w''
  handleEvent w (PlayerAction Rest) = do
    h <- randChoice [1, 2, 0, 0, 0, 0, 0, 4, 0]
    gameLoop w { _wHero = (_wHero w) { _hHealth = if (((_hHealth (_wHero w))) + h) >= 10 then 10 + (getConst (_hClass (_wHero w))) else (_hHealth (_wHero w)) + h } }
  handleEvent w (PlayerAction Quaff) = do
    let h = (_wHero w)
    let inv = (_items h)
    let maxHealth = (getConst (_hClass h)) + 10
    let w' = if (Potion  `elem` inv) then (if (((_hHealth h) + 10) >= maxHealth) then w { _wHero = h { _hHealth = maxHealth } } else w { _wHero = h { _hHealth = (_hHealth h) + 10 } }) else w
    let w'' = w' { _wHero = (_wHero w') { _items = delete Potion (_items (_wHero w')) } }
    gameLoop w''
  handleEvent w (PlayerAction Inspect) = do
    let t = case M.lookup ((flipCoord (_hCoord (_wHero w))), _currentLvl w) (_wInspects w) of {Just xs -> xs; Nothing -> [""]}
    clearScreen
    putStrLn "Message:"
    putStrLn "========"
    mapM_ putStrLn t
    _ <- getInput
    mapM_ (\_ -> putStrLn "                                 ") t
    gameLoop w
  handleEvent w (PlayerAction Buy) = do
    let i = case M.lookup (flipCoord (_hCoord (_wHero w))) (_tileMap w) of
             Nothing -> ' '
             Just c -> c
    let newHero = (_wHero w) { _items = (_items (_wHero w)) ++ [getItem i], _hScore = (_hScore (_wHero w)) + (if i == '$' then 10 else 1), _hMoney = (_hMoney (_wHero w)) - (case M.lookup (flipCoord (_hCoord (_wHero w)), _currentLvl w) (_wShops w) of { Nothing -> 0; Just (_, p) -> p })}
    if (isAShopItem w (flipCoord (_hCoord (_wHero w))) (_currentLvl w)) then gameLoop w { _tileMap = changeTile (flipCoord (_hCoord (_wHero w))) i ' ' (_tileMap w), _wHero = if i == ' ' then (_wHero w) else newHero } else gameLoop w  

  gameLoop :: World -> IO ()
  gameLoop w = do
    if (_mode w) == "ascii" then drawWorld w else drawWorldUnicode w
    let w' = w { _wHero = (checkLevel (_wHero w)) { _hMoney = (if getHeroName w == "JanLawen" then ((_hMoney (_wHero w)) - 1) else (_hMoney (_wHero w))) }, _currEnemies = deadEnemies (_currEnemies w) }
    if (_hHealth (_wHero w)) <= 0 then handleExit w else putStr ""
    event <- getInput
    case event of
      Exit -> handleExit w'
      e -> handleEvent w' e

  drawMap :: M.Map Coord Char -> IO ()
  drawMap m = mapM_ (\((x, y), c) -> do {setCursorPosition y x; putChar c}) l
      where
        l = M.toList m

  drawEnemiesUnicode :: [Enemy] -> IO ()
  drawEnemiesUnicode e
    | length e == 0 = return ()
    | length e == 1 = do {setSGR [SetColor Foreground Dull Green]; setCursorPosition (snd (_eCoord (head e))) (fst (_eCoord (head e))); putStr "💀"; setCursorPosition (snd (_eOldCoord (head e))) (fst (_eOldCoord (head e))); putChar 'M'}
    | otherwise = do {setSGR [SetColor Foreground Dull Green]; setCursorPosition (snd (_eCoord (head e))) (fst (_eCoord (head e))); putStr "💀"; setCursorPosition (snd (_eOldCoord (head e))) (fst (_eOldCoord (head e))); putChar 'M'; drawEnemiesUnicode (tail e)}

  drawEnemies :: [Enemy] -> IO ()
  drawEnemies e
    | length e == 0 = return ()
    | length e == 1 = do {setCursorPosition (snd (_eCoord (head e))) (fst (_eCoord (head e))); putChar 'M'; setCursorPosition (snd (_eOldCoord (head e))) (fst (_eOldCoord (head e))); putChar 'M'}
    | otherwise = do {setCursorPosition (snd (_eCoord (head e))) (fst (_eCoord (head e))); putChar 'M'; setCursorPosition (snd (_eOldCoord (head e))) (fst (_eOldCoord (head e))); putChar 'M'; drawEnemies (tail e)}

  drawItemsUnicode :: [(Coord, Item)] -> IO ()
  drawItemsUnicode m = do
    setSGR [SetColor Foreground Vivid Yellow]
    mapM_ (\((x, y), i) -> do { setCursorPosition y x; putStr (getTile i)}) m
    where
      getTile i = case i of { Potion -> "🍹"; Coin -> "💰" }

  drawItems :: [(Coord, Item)] -> IO ()
  drawItems m = do
    mapM_ (\((x, y), i) -> do { setCursorPosition y x; putChar (getTile i)}) m
    where
      getTile i = case i of { Potion -> 'p'; Coin -> '$' }
    
  drawStats :: Hero -> IO ()
  drawStats h = do
    setSGR [SetColor Foreground Vivid White]
    setCursorPosition 0 50
    putStrLn "Player Stats"
    setCursorPosition 1 50
    putStrLn "============"
    setCursorPosition 2 50
    putStrLn ("Class: " ++ show (_hClass h)) 
    setCursorPosition 3 50
    putStr ("        " ++ "  ")
    setCursorPosition 3 50
    putStrLn ("Health: " ++ show (_hHealth h))
    setCursorPosition 4 50
    putStrLn ("Exp: " ++ show (_hExp h))
    setCursorPosition 5 50
    putStrLn ("Level: " ++ show (_hLvl h))
    setCursorPosition 6 50
    putStrLn ("Money: $" ++ show(_hMoney h))
    setCursorPosition 0 75
    putStrLn ((_hName h) ++ "\'s Score")
    setCursorPosition 1 75
    putStrLn "============"
    setCursorPosition 2 75
    putStrLn (show (_hScore h) ++ " pts")

  drawWorldUnicode :: World -> IO ()
  drawWorldUnicode w = do
    setSGR [SetColor Foreground Vivid White]
    setCursorPosition 0 0
    drawMap (_tileMap w)
    drawEnemiesUnicode (_currEnemies w)
    drawItemsUnicode (M.toList $ _wItems w)
    setCursorPosition (fst $ _hCoord (_wHero w)) (snd $ _hCoord (_wHero w))
    setSGR [SetColor Foreground Vivid Red]
    if getHeroName w == "JanLawen" then putStr "💃" else putChar '@'
    setCursorPosition 0 0
    putChar '-'
    setSGR [SetColor Foreground Vivid White]

  drawWorld :: World -> IO ()
  drawWorld w = do
    setCursorPosition 0 0
    drawMap (_tileMap w)
    drawEnemies (_currEnemies w)
    drawItems (M.toList $ _wItems w)
    setCursorPosition (fst $ _hCoord (_wHero w)) (snd $ _hCoord (_wHero w))
    putChar '@'
    setCursorPosition 0 0
    putChar '-'

  getClass :: String -> IO Class
  getClass n = do
    setCursorPosition 0 0
    putStrLn "Pick a class"
    putStrLn "============"
    putStrLn "a - Knight"
    putStrLn "b - Thief"
    putStrLn "c - Substitute Teacher"
    putStrLn "* - Random Class"
    c <- getChar
    let c' = case c of
              'a' -> return knight
              'b' -> return thief
              'c' -> return sub
              '*' -> randChoice [knight, thief, sub]
    if (n == "JanLawen") then return polkaKing else c'
    
  main :: IO ()
  main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Vauxhall"
    argLength <- length <$> getArgs
    name <- if argLength > 0 then head <$> getArgs else return "Moz"
    mode <- if argLength > 1 then (head . drop 1) <$> getArgs else return "ascii"
    clearScreen
    setCursorPosition 0 0
    mapM_ (\s -> putStrLn s) titleStrings
    _ <- getInput
    clearScreen
    c <- getClass name
    clearScreen
    is <- spawnItems (mapWalls wall1) 5
    let w = World {
                   _mode = mode, 
                   _wHero = Hero {_hName = name, _hCoord = (2, 1), _hOldCoord = (30, 0), _hHealth = 10 + (getConst c), _hDmg = getStr c, _hExp = 0, _hLvl = 1, _hClass = c, _items = [], _hScore = 0, _hMoney = (if name == "JanLawen" then 999 else 0) }, 
                   _walls = wall1,
                   _currentLvl = "wall1",
                   _tileMap = mapWalls wall1,
                   _wItems = M.fromList is,
                   _wEnemies = enemiesList,
                   _currEnemies = case M.lookup "wall1" enemiesList of {Nothing -> []; Just es -> es},
                   _wStairs = [],
                   _wInspects = inspectList,
                   _wShops = shopList
                  }
    if mode == "ascii" then drawWorld w else drawWorldUnicode w
    gameLoop w