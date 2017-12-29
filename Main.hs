module Main where
  import Prelude hiding (Either(..))
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
    [?] Equip/De-Equip items
    [] Leveling/Classes/Attributes
    [X] Way to check player status (To right side of map)
    [X] Up/Down Staircases
    [] Specific enemies for each floor!!!!!
    [] Clean up code!!!!
    [] Fix Coordinate problems (get rid of flipCoords, make a standard format for coords)
    [] Change wallsList to just accept a [(String, [String])] and then put it all together
    [X] Score kept throughout game 
    [X] Score displayed at end/death
        - Killing enemies [X]
        - Picking up items [X]
        - Picking up gold [X]
    [] Hunger System
  -}

  wall1 :: [String]
  wall1 = ["-----------------------     ----------",
           "|<           s        |     |    >   |",
           "|               O     ---|---    $   |",
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
  wall4 = ["--------------------------------------",
          "|                                    |",
          "|                  |                 |",
          "|                  |               < |",
          "|                $-+-$               |",
          "|                  |                 |",
          "--------------------------------------"]

  testEnemy :: Enemy
  testEnemy = Enemy { eCoord = (33, 3), eOldCoord = (0, 0), eHealth = 10 }

  wallsList :: M.Map String [String]
  wallsList = M.insert "wall3" wall3 (M.insert "wall2" wall2 (M.insert "wall1" wall1 M.empty))

  enemiesList :: M.Map String [Enemy]
  enemiesList = M.insert "wall 3" [testEnemy] (M.insert "wall2" [testEnemy] (M.insert "wall1" [testEnemy] M.empty))

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

  debug :: String -> IO ()
  debug s = appendFile "test.txt" (s ++ "\n")

  flipCoord :: Coord -> Coord
  flipCoord (x, y) = (y, x)

  getItem :: Char -> Item
  getItem 's' = Sword
  getItem 'O' = Shield
  getItem 'p' = Potion
  getItem '$' = Coin
  getItem _ = Null

  getVect :: Direction -> Coord
  getVect Up = (-1, 0) |+| uConst
  getVect Down = (1, 0) |+| dConst
  getVect Left = (0, -1) |+| lConst
  getVect Right = (0, 1) |+| rConst

  isImpassible :: M.Map Coord Char -> Coord -> Bool
  isImpassible m coord = case M.lookup coord m of
                          Nothing -> True
                          Just c -> case c of
                                     '-' -> True
                                     '|' -> True
                                     '+' -> True
                                     _ -> False

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
    dir <- randChoice [Up, Down, Left, Right]
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
    ems <- moveEnemies w (wEnemies w)
    let eFighting = findEnemyByCoord (wEnemies w) (flipCoord newHCoord)
    case eFighting of
     Just e -> gameLoop w { wHero = (wHero w) { hHealth = (hHealth (wHero w)) - 1, hScore = (hScore (wHero w)) + 2 }, wEnemies = (updateEnemies (wEnemies w) e newHCoord)}
     Nothing -> gameLoop w { wHero = if (isImpassible (tileMap w) (flipCoord newHCoord)) then (wHero w) else newH, wEnemies = ems }
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
    let newHero = (wHero w) { items = (items (wHero w)) ++ [getItem i], hScore = (hScore (wHero w)) + (if i == '$' then 10 else 1) }
    let oldHero = (wHero w)
    gameLoop w { tileMap = changeTile (flipCoord (hCoord (wHero w))) i ' ' (tileMap w), wHero = if i == ' ' then oldHero else newHero }
  handleEvent w (PlayerAction ShowInv) = do
    setCursorPosition 0 0
    putStrLn "Inventory"
    putStrLn "========="
    mapM_ putStrLn (map show $ items (wHero w))
    _ <- getInput
    setCursorPosition 0 0
    mapM_ (\_ -> putStrLn "               ") (map show $ items (wHero w))
    putStrLn "                 "
    putStrLn "                 "
    gameLoop w
  handleEvent w (PlayerAction ShowStats) = do
    drawStats (wHero w)
    _ <- getInput
    setCursorPosition 0 50
    putStrLn "                "
    setCursorPosition 1 50
    putStrLn "                "
    setCursorPosition 2 50
    putStrLn "                "
    setCursorPosition 3 50
    putStrLn "                "
    setCursorPosition 4 50
    putStrLn "                "
    setCursorPosition 5 50
    putStrLn "                "
    setCursorPosition 6 50
    putStrLn "                "
    setCursorPosition 7 50
    putStrLn "                "
    setCursorPosition 8 50
    putStrLn "                "
    setCursorPosition 0 75
    putStrLn "                "
    setCursorPosition 1 75
    putStrLn "                "
    setCursorPosition 2 75
    putStrLn "                "
    gameLoop w
  handleEvent w (PlayerAction GoDown) = do
    let t = case M.lookup (flipCoord (hCoord (wHero w))) (tileMap w) of
             Nothing -> ' '
             Just c -> c
    let nextStr = getNextLvl (currentLvl w)
    let nextWalls = case M.lookup nextStr wallsList of
                     Nothing -> (walls w)
                     Just ch -> ch
    let nextEnemies = case M.lookup nextStr enemiesList of
                       Nothing -> (wEnemies w)
                       Just es -> es
    let w' = if t == '>' then w { walls = (nextWalls), currentLvl = nextStr, tileMap = mapWalls nextWalls, wEnemies = nextEnemies } else w
    gameLoop w'
  handleEvent w (PlayerAction GoUp) = do
    let t = case M.lookup (flipCoord (hCoord (wHero w))) (tileMap w) of
              Nothing -> ' '
              Just c -> c
    let nextStr = getLastLvl (currentLvl w)
    let lastWalls = case M.lookup nextStr wallsList of
                      Nothing -> (walls w)
                      Just ch -> ch
    let lastEnemies = case M.lookup nextStr enemiesList of
                       Nothing -> (wEnemies w)
                       Just es -> es
    let w' = if t == '<' then w { walls = (lastWalls), currentLvl = nextStr, tileMap = mapWalls lastWalls, wEnemies = lastEnemies } else w
    gameLoop w'

  gameLoop :: World -> IO ()
  gameLoop w = do
    drawWorld w
    let w' = w { wEnemies = deadEnemies (wEnemies w) }
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
    drawEnemies (wEnemies w)
    setCursorPosition (fst $ hCoord (wHero w)) (snd $ hCoord (wHero w))
    putChar '@'
    setCursorPosition 0 0
    putChar '-'

  main :: IO ()
  main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Vauxhall"
    name <- head <$> getArgs
    clearScreen
    let w = World { wHero = Hero {hName = name, hCoord = (2, 1), hOldCoord = (30, 0), hHealth = 1000, hExp = 0, hLvl = 1, hClass = knight, items = [], hScore = 0 }, 
                    walls = wall1,
                    currentLvl = "wall1",
                    tileMap = wall1Mapped,
                    wEnemies = [testEnemy]
                  }
    drawWorld w
    gameLoop w