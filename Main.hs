module Main where
  import Prelude hiding (Either(..))
  import System.Console.ANSI
  import System.IO
  import System.Random
  import qualified Data.Map.Strict as M
  import Types

  {- Written By Henning Tonko ☭ -}

  {-
  TODO:
    [X] Add money
    [] Keep enemy within impassible objects (Check coords and make sure they are both correct)
    [] Implement Enemy and Player attacking
    [] Equip/De-Equip items
    [] Leveling/Classes/Attributes
    [X] Way to check player status (To right side of map)
    [] Up/Down Staircases
    [] Score displayed at end/death
        - Killing enemies
        - Picking up items
        - Picking up gold
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

  wall1Mapped :: M.Map Coord Char
  wall1Mapped = foldl M.union M.empty $ mapWorld wall1 0

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
    debug ("New: " ++ show newCoord)
    debug ("Old: " ++ show (eCoord e))
    return e { eCoord = if (isImpassible (tileMap w) (newCoord)) then (flipCoord (eCoord e)) else newCoord, eOldCoord = (eOldCoord e) }

  moveEnemies :: World -> [Enemy] -> IO [Enemy]
  moveEnemies w es = mapM (moveEnemy w) es

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
      _   -> getInput

  handleExit :: IO ()
  handleExit = do
    clearScreen
    setCursorPosition 0 0
    showCursor
    setSGR [Reset]
    putStrLn "Thanks for playing!"

  handleEvent :: World -> Event -> IO ()
  handleEvent w (Dir d) = do
    ems <- moveEnemies w (wEnemies w)
    gameLoop w { wHero = if (isImpassible (tileMap w) (flipCoord newHCoord)) then (wHero w) else newH, wEnemies = ems }
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
    let newHero = (wHero w) { items = (items (wHero w)) ++ [getItem i] }
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
    gameLoop w

  gameLoop :: World -> IO ()
  gameLoop w = do
    drawWorld w
    event <- getInput
    case event of
      Exit -> handleExit
      e -> handleEvent w e

  drawMap :: M.Map Coord Char -> IO ()
  drawMap m = mapM_ (\((x, y), c) -> do {setCursorPosition y x; putChar c}) l
      where
        l = M.toList m

  drawEnemies :: [Enemy] -> IO ()
  drawEnemies e
    | length e == 1 = do {setCursorPosition (snd (eCoord (head e))) (fst (eCoord (head e))); putChar 'M'; setCursorPosition (snd (eOldCoord (head e))) (fst (eOldCoord (head e))); putChar 'M'}
    | otherwise = do {setCursorPosition (snd (eCoord (head e))) (fst (eCoord (head e))); putChar 'M'; setCursorPosition (snd (eOldCoord (head e))) (fst (eOldCoord (head e))); putChar 'M'; drawEnemies (tail e)}

  drawStats :: Hero -> IO ()
  drawStats h = do
    setCursorPosition 0 50
    putStrLn "Player Stats"
    setCursorPosition 1 50
    putStrLn "============"
    setCursorPosition 2 50
    putStrLn ("Health: " ++ show (hHealth h))
    setCursorPosition 3 50
    putStrLn ("Exp: " ++ show (hExp h))
    setCursorPosition 4 50
    putStrLn ("Level: " ++ show (hLvl h))

  drawWorld :: World -> IO ()
  drawWorld w = do
    setCursorPosition 0 0
    drawMap (tileMap w)
    drawEnemies (wEnemies w)
    drawStats (wHero w)
    setCursorPosition (fst $ hOldCoord (wHero w)) (snd $ hOldCoord (wHero w))
    putChar ' '
    setCursorPosition (fst $ hCoord (wHero w)) (snd $ hCoord (wHero w))
    putChar '@'

  main :: IO ()
  main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Vauxhall"
    clearScreen
    let w = World { wHero = Hero {hCoord = (1, 1), hOldCoord = (30, 0), hHealth = 10, hExp = 0, hLvl = 1, items = []}, 
                    walls = wall1,
                    tileMap = wall1Mapped,
                    wEnemies=[Enemy {eCoord = (33, 3), eOldCoord = (0,0), eHealth = 10}]
                  }
    drawWorld w
    gameLoop w