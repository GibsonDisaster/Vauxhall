module Main where
  import Prelude hiding (Either(..))
  import Data.List (delete)
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
    [] Add a shop or a use for money
    [] Random item placement
    [X] Add health potions
    [X] Remove gear from the ground
    [?] Multiple kinds of Enemies
    [] Make a better title screen
    [] Make it FUN!!!!
    [] Boss at the end called "The Relayer"
    [] Branching paths?
    [X] Change wallsList to just accept a [(String, [String])] and then put it all together
    [X] Score kept throughout game 
    [X] Score displayed at end/death
        - Killing enemies [X]
        - Picking up items [X]
        - Picking up gold [X]
  -}

  titleStrings :: [String]
  titleStrings = ["{===================}                              ",
                  "                                                  ",
                  "       Vauxhall                                   ",
                  "                                                   ",
                  "      Written By:                                  ",
                  "        Henning                                    ",
                  "         Tonko                                     ",
                  "                                                  ",
                  "                                                   ",
                  "     \'a\' to begin                                ",
                  "{===================}"]

  wall1 :: [String]
  wall1 = ["-----------------------     ----------",
           "|<                    |     |    >   |",
           "|               p     ---|---    $   |",
           "|          p             +           |",
           "|                 $   ---|---        |",
           "|       $             |     | $      |",
           "-----------------------     ----------"]

  wall2 :: [String]
  wall2 = ["--------------------------------------",
           "|>                    |$$$$$|    <   |",
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
  spawnEnemy c h = Enemy { eCoord = c, eOldCoord = (0, 0), eHealth = h }

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

  knight :: Class
  knight = Knight { kConst = 15, kStr = 4, kDex = 7, kInt = 3 }

  thief :: Class
  thief = Thief { tConst = 8, tStr = 3, tDex = 10, tInt = 6 }

  sub :: Class
  sub = Sub { sConst = 10, sStr = 2, sDex = 15, sInt = 1 }

  polkaKing :: Class
  polkaKing = PolkaKing { pConst = 17, pStr = 2, pDex = 11, pInt = 900 }

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
  isAShopItem w c l = case M.lookup (c, l) (wShops w) of
                       Nothing -> False
                       Just t -> True

  findEnemyByCoord :: [Enemy] -> Coord -> Maybe Enemy
  findEnemyByCoord es c
    | length es == 0 = Nothing
    | length es == 1 = if (eCoord (head es)) == (eCoord dummy) then Just (head es) else Nothing
    | (eCoord (head es)) == (eCoord dummy) = Just (head es)
    | otherwise = findEnemyByCoord (tail es) c
      where
        dummy = Enemy { eCoord = c, eOldCoord = (1, 1), eHealth = 10 }

  getNextLvl :: String -> String
  getNextLvl lvl = "wall" ++ show nextNum
    where
      nextNum = (read (drop 4 lvl) :: Int) + 1

  getLastLvl :: String -> String
  getLastLvl lvl = "wall" ++ show lastNum
    where
      lastNum = (read (drop 4 lvl) :: Int) - 1

  wall1Mapped :: M.Map Coord Char
  wall1Mapped = foldl M.union M.empty $ mapWorld wall1 0

  wall2Mapped :: M.Map Coord Char
  wall2Mapped = foldl M.union M.empty $ mapWorld wall2 0

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

  (|+|) :: Coord -> Coord -> Coord
  (|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  (|-|) :: Coord -> Coord -> Coord
  (|-|) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

  dropQuotes :: String -> String
  dropQuotes str = (drop 1 . reverse . drop 1 . reverse) str

  getConst :: Class -> Int
  getConst c
    | c == knight = kConst c
    | c == thief = tConst c
    | c == sub = sConst c
    | c == polkaKing = pConst c
    
  getStr :: Class -> Int
  getStr c
    | c == knight = kStr c
    | c == thief = tStr c
    | c == sub = sStr c
    | c == polkaKing = pStr c

  getDex :: Class -> Int
  getDex c
    | c == knight = kDex c
    | c == thief = tDex c
    | c == sub = sDex c
    | c == polkaKing = pDex c

  getInt :: Class -> Int
  getInt c
    | c == knight = kInt c
    | c == thief = tInt c
    | c == sub = sInt c
    | c == polkaKing = pInt c

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
    let newCoord = (flipCoord (getVect dir) |+| (eCoord e))
    return e { eCoord = if (isImpassible (tileMap w) (newCoord)) then (eCoord e) else newCoord, eOldCoord = (eOldCoord e) }

  moveEnemies :: World -> [Enemy] -> IO [Enemy]
  moveEnemies w es = mapM (moveEnemy w) es

  updateEnemies :: [Enemy] -> Enemy -> Coord -> [Enemy]
  updateEnemies es e c = let foundE = case findEnemyByCoord es (eCoord e) of
                                       Just e -> e
                                       Nothing -> undefined
                             removeE = filter (\en -> (eHealth en > 0)) (filter (/= foundE) es)
                             newE = removeE ++ [e { eHealth = (eHealth e) - 1 }] in newE

  deadEnemies :: [Enemy] -> [Enemy]
  deadEnemies es
    | length es == 0 = []
    | length es == 1 = if (eHealth (head es)) <= 0 then [] else [(head es)]
    | otherwise = (if (eHealth (head es)) <= 0 then [] else [(head es)]) ++ deadEnemies (tail es)

  checkLevel :: Hero -> Hero
  checkLevel h
    | (hExp h) >= 20 = h { hExp = 0, hLvl = (hLvl h) + 1 }
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
    putStrLn (dropQuotes (show $ hName (wHero w)))
    setCursorPosition 13 0
    putStrLn ("Your Score was: " ++ (show $ hScore (wHero w)))
    showCursor
    setSGR [Reset]

  handleEvent :: World -> Event -> IO ()
  handleEvent w (Dir d) = do
    ems <- moveEnemies w (currEnemies w)
    let eFighting = findEnemyByCoord (currEnemies w) (flipCoord newHCoord)
    case eFighting of
     Just e -> gameLoop w { wHero = (wHero w) { hHealth = (hHealth (wHero w)) - 1, hExp = (hExp (wHero w)) + 2, hScore = (hScore (wHero w)) + 2 }, currEnemies = (updateEnemies (currEnemies w) e newHCoord)}
     Nothing -> gameLoop w { wHero = if (isImpassible (tileMap w) (flipCoord newHCoord)) then (wHero w) else newH, currEnemies = ems }
    where
      oldH = hCoord (wHero w)
      newHCoord = case d of
                   Left -> (0, -1) |+| oldH
                   Right -> (0, 1) |+| oldH
                   Up -> (-1, 0) |+| oldH
                   Down -> (1, 0) |+| oldH
      newH = (wHero w) { hCoord = newHCoord, hOldCoord = oldH, hHealth = hHealth (wHero w), items = items (wHero w)}
  handleEvent w (PlayerAction OpenDoor) = do
    dir <- getInput
    let t = case dir of
             Dir Right -> (0, 1) |+| (flipCoord (hCoord (wHero w)) |+| rConst)
             Dir Left -> (0, -1) |+| (flipCoord (hCoord (wHero w)) |+| lConst)
             Dir Up -> (-1, 0) |+| (flipCoord (hCoord (wHero w)) |+| uConst)
             Dir Down -> (1, 0) |+| (flipCoord (hCoord (wHero w)) |+| dConst)
             _ -> (hCoord (wHero w))
    gameLoop w { tileMap = changeTile t '+' 'o' (tileMap w)}
  handleEvent w (PlayerAction CloseDoor) = do
    dir <- getInput
    let t = case dir of
             Dir Right -> (0, 1) |+| (flipCoord (hCoord (wHero w)) |+| rConst)
             Dir Left -> (0, -1) |+| (flipCoord (hCoord (wHero w)) |+| lConst)
             Dir Up -> (-1, 0) |+| (flipCoord (hCoord (wHero w)) |+| uConst)
             Dir Down -> (1, 0) |+| (flipCoord (hCoord (wHero w)) |+| dConst)
             _ -> (hCoord (wHero w))
    gameLoop w { tileMap = changeTile t 'o' '+' (tileMap w) }
  handleEvent w (PlayerAction PickUp) = do
    let i = case M.lookup (flipCoord (hCoord (wHero w))) (tileMap w) of
             Nothing -> ' '
             Just c -> c
    let newHero = (wHero w) { items = (items (wHero w)) ++ [getItem i], hScore = (hScore (wHero w)) + (if i == '$' then 10 else 1), hMoney = (hMoney (wHero w)) + (if i == '$' then (5) else 0)}
    let oldHero = (wHero w)
    if (isAShopItem w (flipCoord (hCoord (wHero w))) (currentLvl w)) then gameLoop w else gameLoop w { tileMap = changeTile (flipCoord (hCoord (wHero w))) i ' ' (tileMap w), wHero = if i == ' ' then oldHero else newHero }    
  handleEvent w (PlayerAction ShowInv) = do
    setCursorPosition 0 0
    putStrLn "Inventory"
    putStrLn "========="
    mapM_ putStrLn (map show $ filter (/= Coin) (items (wHero w)))
    _ <- getInput
    setCursorPosition 0 0
    mapM_ (\_ -> putStrLn "               ") (map show $ items (wHero w))
    putStrLn "                 "
    putStrLn "                 "
    gameLoop w
  handleEvent w (PlayerAction ShowStats) = do
    drawStats (wHero w)
    _ <- getInput
    clearScreen
    gameLoop w
  handleEvent w (PlayerAction GoDown) = do
    let t = case M.lookup (flipCoord (hCoord (wHero w))) (tileMap w) of
             Nothing -> ' '
             Just c -> c
    let nextStr = getNextLvl (currentLvl w)
    let nextWalls = case M.lookup nextStr wallsList of
                     Nothing -> (walls w)
                     Just ch -> ch
    let nextEnemies = case M.lookup nextStr (wEnemies w) of
                       Nothing -> (currEnemies w)
                       Just es -> es
    let w' = if t == '>' then w { walls = (nextWalls), currentLvl = nextStr, tileMap = mapWalls nextWalls, currEnemies = nextEnemies, wEnemies = M.insert (currentLvl w) (currEnemies w) (wEnemies w) } else w
    gameLoop w'
  handleEvent w (PlayerAction GoUp) = do
    let t = case M.lookup (flipCoord (hCoord (wHero w))) (tileMap w) of
              Nothing -> ' '
              Just c -> c
    let nextStr = getLastLvl (currentLvl w)
    let lastWalls = case M.lookup nextStr wallsList of
                      Nothing -> (walls w)
                      Just ch -> ch
    let lastEnemies = case M.lookup nextStr (wEnemies w) of
                       Nothing -> (currEnemies w)
                       Just es -> es
    let w' = if t == '<' then w { walls = (lastWalls), currentLvl = nextStr, tileMap = mapWalls lastWalls, currEnemies = lastEnemies, wEnemies = M.insert (currentLvl w) (currEnemies w) (wEnemies w) } else w
    gameLoop w'
  handleEvent w (PlayerAction Rest) = do
    h <- randChoice [1, 2, 0, 0, 0, 0, 0, 4, 0]
    gameLoop w { wHero = (wHero w) { hHealth = if (((hHealth (wHero w))) + h) >= 10 then 10 + (getConst (hClass (wHero w))) else (hHealth (wHero w)) + h } }
  handleEvent w (PlayerAction Quaff) = do
    let h = (wHero w)
    let inv = (items h)
    let maxHealth = (getConst (hClass h)) + 10
    let w' = if (Potion  `elem` inv) then (if (((hHealth h) + 10) >= maxHealth) then w { wHero = h { hHealth = maxHealth } } else w { wHero = h { hHealth = (hHealth h) + 10 } }) else w
    let w'' = w' { wHero = (wHero w') { items = delete Potion (items (wHero w')) } }
    gameLoop w''
  handleEvent w (PlayerAction Inspect) = do
    let t = case M.lookup ((flipCoord (hCoord (wHero w))), currentLvl w) (wInspects w) of {Just xs -> xs; Nothing -> [""]}
    clearScreen
    putStrLn "Message:"
    putStrLn "========"
    mapM_ putStrLn t
    _ <- getInput
    mapM_ (\_ -> putStrLn "                                 ") t
    gameLoop w
  handleEvent w (PlayerAction Buy) = do
    let i = case M.lookup (flipCoord (hCoord (wHero w))) (tileMap w) of
             Nothing -> ' '
             Just c -> c
    let newHero = (wHero w) { items = (items (wHero w)) ++ [getItem i], hScore = (hScore (wHero w)) + (if i == '$' then 10 else 1), hMoney = (hMoney (wHero w)) - (case M.lookup (flipCoord (hCoord (wHero w)), currentLvl w) (wShops w) of { Nothing -> 0; Just (_, p) -> p })}
    if (isAShopItem w (flipCoord (hCoord (wHero w))) (currentLvl w)) then gameLoop w { tileMap = changeTile (flipCoord (hCoord (wHero w))) i ' ' (tileMap w), wHero = if i == ' ' then (wHero w) else newHero } else gameLoop w  

  gameLoop :: World -> IO ()
  gameLoop w = do
    drawWorld w
    let w' = w { wHero = (checkLevel (wHero w)) { hMoney = (if (hName (wHero w)) == "JanLawen" then ((hMoney (wHero w)) - 1) else (hMoney (wHero w))) }, currEnemies = deadEnemies (currEnemies w) }
    if (hHealth (wHero w)) <= 0 then handleExit w else putStr ""
    event <- getInput
    case event of
      Exit -> handleExit w'
      e -> handleEvent w' e

  drawMap :: M.Map Coord Char -> IO ()
  drawMap m = mapM_ (\((x, y), c) -> do {setCursorPosition y x; putChar c}) l
      where
        l = M.toList m

  drawEnemies :: [Enemy] -> IO ()
  drawEnemies e
    | length e == 0 = return ()
    | length e == 1 = do {setCursorPosition (snd (eCoord (head e))) (fst (eCoord (head e))); putChar 'M'; setCursorPosition (snd (eOldCoord (head e))) (fst (eOldCoord (head e))); putChar 'M'}
    | otherwise = do {setCursorPosition (snd (eCoord (head e))) (fst (eCoord (head e))); putChar 'M'; setCursorPosition (snd (eOldCoord (head e))) (fst (eOldCoord (head e))); putChar 'M'; drawEnemies (tail e)}

  drawStats :: Hero -> IO ()
  drawStats h = do
    setCursorPosition 0 50
    putStrLn "Player Stats"
    setCursorPosition 1 50
    putStrLn "============"
    setCursorPosition 2 50
    putStrLn ("Class: " ++ show (hClass h)) 
    setCursorPosition 3 50
    putStr ("        " ++ "  ")
    setCursorPosition 3 50
    putStrLn ("Health: " ++ show (hHealth h))
    setCursorPosition 4 50
    putStrLn ("Exp: " ++ show (hExp h))
    setCursorPosition 5 50
    putStrLn ("Level: " ++ show (hLvl h))
    setCursorPosition 6 50
    putStrLn ("Money: $" ++ show(hMoney h))
    setCursorPosition 0 75
    putStrLn ((hName h) ++ "\'s Score")
    setCursorPosition 1 75
    putStrLn "============"
    setCursorPosition 2 75
    putStrLn (show (hScore h) ++ " pts")

  drawWorld :: World -> IO ()
  drawWorld w = do
    setCursorPosition 0 0
    drawMap (tileMap w)
    drawEnemies (currEnemies w)
    setCursorPosition (fst $ hCoord (wHero w)) (snd $ hCoord (wHero w))
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

  showTitleScreen :: IO ()
  showTitleScreen = do { setCursorPosition 0 0; mapM_ (\s -> putStrLn s) titleStrings }
  
  main :: IO ()
  main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Vauxhall"
    name <- head <$> getArgs
    clearScreen
    showTitleScreen
    _ <- getInput
    clearScreen
    c <- getClass name
    clearScreen
    let w = World { 
                   wHero = Hero {hName = name, hCoord = (2, 1), hOldCoord = (30, 0), hHealth = 10 + (getConst c), hDmg = getStr c, hExp = 0, hLvl = 1, hClass = c, items = [], hScore = 0, hMoney = (if name == "JanLawen" then 999 else 0) }, 
                   walls = wall1,
                   currentLvl = "wall1",
                   tileMap = mapWalls wall1,
                   wEnemies = enemiesList,
                   currEnemies = case M.lookup "wall1" enemiesList of {Nothing -> []; Just es -> es},
                   wInspects = inspectList,
                   wShops = shopList
                  }
    drawWorld w
    gameLoop w