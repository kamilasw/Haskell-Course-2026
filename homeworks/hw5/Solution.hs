import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Text.Read (readMaybe)

-- task 1

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
    deriving (Show, Eq) 


changeStack :: Instr -> [Int] -> [Int]

changeStack (PUSH x) stack =
  x : stack

changeStack POP (_ : rest) =
  rest
changeStack POP stack =
  stack

changeStack DUP (x : rest) =
  x : x : rest
changeStack DUP stack =
  stack

changeStack SWAP (x : y : rest) =
  y : x : rest
changeStack SWAP stack =
  stack

changeStack ADD (x : y : rest) =
  (x + y) : rest
changeStack ADD stack =
  stack

changeStack MUL (x : y : rest) =
  (x * y) : rest
changeStack MUL stack =
  stack

changeStack NEG (x : rest) =
  (-x) : rest
changeStack NEG stack =
  stack


execInstr :: Instr -> State [Int] ()
execInstr instr =
  modify (changeStack instr)


execProg :: [Instr] -> State [Int] ()
execProg [] =
  return ()
execProg (instr : rest) = do
  execInstr instr
  execProg rest


runProg :: [Instr] -> [Int]
runProg prog =
  execState (execProg prog) []


-- task 2

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr   -- bind the value of the expression to the name, return that value
  | Seq  Expr Expr       -- evaluate the left, then the right; return the value of the right
  deriving(Show, Eq)


eval :: Expr -> State (Map String Int) Int
eval (Num n) =
  return n

eval (Var name) = do
  env <- get
  return (env Map.! name)

eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)

eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)

eval (Neg e) = do
  v <- eval e
  return (-v)

eval (Assign name e) = do
  value <- eval e
  modify (Map.insert name value)
  return value

eval (Seq e1 e2) = do
  eval e1
  eval e2

runEval :: Expr -> Int
runEval expr =
  evalState (eval expr) Map.empty


-- task 3
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get

  case Map.lookup (i, j) cache of
    Just result ->
      return result

    Nothing -> do
      result <-
        if i == 0 then
          return j

        else if j == 0 then
          return i

        else if xs !! (i - 1) == ys !! (j - 1) then
          editDistM xs ys (i - 1) (j - 1)

        else do
          deletion <- editDistM xs ys (i - 1) j
          insertion <- editDistM xs ys i (j - 1)
          substitution <- editDistM xs ys (i - 1) (j - 1)

          return (1 + minimum [deletion, insertion, substitution])

      modify (Map.insert (i, j) result)
      return result


editDistance :: String -> String -> Int
editDistance xs ys =
  evalState (editDistM xs ys (length xs) (length ys)) Map.empty


-- THE GAME

data GameState = GameState
  { position           :: Int
  , energy             :: Int
  , score              :: Int
  , pathName           :: String
  , collectedTreasures :: [(String, Int)]
  } deriving (Show)

type AdventureGame a = StateT GameState IO a

-- task 4
movePlayer :: Int -> AdventureGame Int
movePlayer diceRoll = do
  game <- get

  let newPosition = position game + diceRoll
  let newEnergy = energy game - diceRoll

  put game
    { position = newPosition
    , energy = max 0 newEnergy
    }

  liftIO $ putStrLn ("You moved " ++ show diceRoll ++ " spaces.")

  return diceRoll

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  liftIO $ putStrLn "You reached a decision point."

  choice <- liftIO $ getPlayerChoice options

  game <- get
  put game { pathName = choice }

  liftIO $ putStrLn ("You chose: " ++ choice)

  return choice

-- task 5

goalPosition :: Int
goalPosition =
  12

treasures :: [(String, Int)]
treasures =
  [ ("forest", 6)
  , ("cave", 8)
  , ("river", 5)
  ]

traps :: [(String, Int)]
traps =
  [ ("forest", 9)
  , ("cave", 6)
  , ("river", 8)
  ]

obstacles :: [(String, Int)]
obstacles =
  [ ("forest", 4)
  , ("cave", 5)
  , ("river", 7)
  ]

isTreasure :: String -> Int -> Bool
isTreasure path pos =
  (path, pos) `elem` treasures

isTrap :: String -> Int -> Bool
isTrap path pos =
  (path, pos) `elem` traps

isObstacle :: String -> Int -> Bool
isObstacle path pos =
  (path, pos) `elem` obstacles

handleLocation :: AdventureGame Bool
handleLocation = do
  game <- get

  let pos = position game
  let path = pathName game

  if pos >= goalPosition then do
    liftIO $ putStrLn "You reached the main treasure!"
    return True

  else if path == "start" && pos >= 3 then do
    choice <- makeDecision ["forest", "cave", "river"]
    liftIO $ putStrLn ("You entered the " ++ choice ++ " path.")
    return False

  else if isObstacle path pos then do
    liftIO $ putStrLn "Obstacle! You lose 2 energy and move back 1 space."

    put game
      { position = max 0 (pos - 1)
      , energy = max 0 (energy game - 2)
      }

    return False

  else if isTrap path pos then do
    liftIO $ putStrLn "Trap! You lose 5 points."

    put game
      { score = max 0 (score game - 5)
      }

    return False

  else if isTreasure path pos then do
    let treasureKey = (path, pos)

    if treasureKey `elem` collectedTreasures game then do
      liftIO $ putStrLn "This treasure was already collected."
      return False

    else do
      liftIO $ putStrLn "You found an intermediate treasure! +10 points."

      put game
        { score = score game + 10
        , collectedTreasures = treasureKey : collectedTreasures game
        }

      return False

  else do
    liftIO $ putStrLn "Nothing special here."
    return False

playTurn :: AdventureGame Bool
playTurn = do
  game <- get

  if energy game <= 0 then do
    liftIO $ putStrLn "You have no energy left."
    return True

  else do
    diceRoll <- liftIO getDiceRoll

    movePlayer diceRoll

    reachedGoal <- handleLocation

    newGame <- get
    liftIO $ displayGameState newGame

    if reachedGoal then
      return True

    else if energy newGame <= 0 then do
      liftIO $ putStrLn "You ran out of energy."
      return True

    else
      return False

playGame :: AdventureGame ()
playGame = do
  ended <- playTurn

  if ended then
    liftIO $ putStrLn "Game over."

  else
    playGame


-- task 6
getDiceRoll :: IO Int
getDiceRoll = do
  putStrLn ""
  putStrLn "Roll the dice!"
  putStrLn "Enter a number from 1 to 6:"

  input <- getLine

  case readMaybe input of
    Just number ->
      if number >= 1 && number <= 6 then
        return number
      else do
        putStrLn "Invalid dice roll. Please enter a number from 1 to 6."
        getDiceRoll

    Nothing -> do
      putStrLn "Invalid input. Please enter a number."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState game = do
  putStrLn ""
  putStrLn "=============================="
  putStrLn "Current game state:"
  putStrLn ("Path:     " ++ pathName game)
  putStrLn ("Position: " ++ show (position game))
  putStrLn ("Energy:   " ++ show (energy game))
  putStrLn ("Score:    " ++ show (score game))
  putStrLn ("Treasures collected: " ++ show (length (collectedTreasures game)))
  putStrLn "=============================="

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn ""
  putStrLn "Choose one of these options:"

  mapM_ (\option -> putStrLn ("- " ++ option)) options

  choice <- getLine

  if choice `elem` options then
    return choice
  else do
    putStrLn "Invalid choice. Please type one of the listed options."
    getPlayerChoice options

initialGameState :: GameState
initialGameState =
  GameState
    { position = 0
    , energy = 20
    , score = 0
    , pathName = "start"
    , collectedTreasures = []
    }

main :: IO ()
main = do
  putStrLn "Welcome to Treasure Hunters!"
  runStateT playGame initialGameState
  return ()