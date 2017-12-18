module Main where
  import Prelude hiding (Either(..))
  import System.Console.ANSI
  import System.IO
  import qualified Data.Map.Strict as M
  import Types

  wall1 :: [String]
  wall1 = ["-----------------------     ----------",
           "|                     |     |        |",
           "|                     ---|---        |",
           "|                        +           |",
           "|                     ---|---        |",
           "|                     |     |        |",
           "-----------------------     ----------"]

  rConst :: Coord
  rConst = (1, -1)

  lConst :: Coord
  lConst = (-1, 1)

  uConst :: Coord
  uConst = (1, -1)

  dConst :: Coord
  dConst = (-1, 1)

  debug :: String -> IO ()
  debug s = appendFile "test.txt" s

  flipCoord :: Coord -> Coord
  flipCoord (x, y) = (y, x)

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
    gameLoop w { wHero = if (isImpassible (tileMap w) (flipCoord newHCoord)) then (wHero w) else newH }
    where
      oldH = hCoord (wHero w)
      newHCoord = case d of
                   Left -> (0, -1) |+| oldH
                   Right -> (0, 1) |+| oldH
                   Up -> (-1, 0) |+| oldH
                   Down -> (1, 0) |+| oldH
      newH = Hero {hCoord = newHCoord, hOldCoord = oldH}
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

  drawWorld :: World -> IO ()
  drawWorld w = do
    setCursorPosition 0 0
    drawMap (tileMap w)
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
    let w = World { wHero = Hero {hCoord = (1, 1), hOldCoord = (30, 0)}, walls = wall1, tileMap = wall1Mapped }
    drawWorld w
    gameLoop w