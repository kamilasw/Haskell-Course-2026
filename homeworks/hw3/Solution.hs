import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.List (permutations)
import Control.Monad.Writer

-- task 1 maze navigation

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)

type Maze = Map.Map Pos (Map.Map Dir Pos)

-- a
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    neighbours <- Map.lookup pos maze
    Map.lookup dir neighbours

-- b
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos [] = Just pos
followPath maze pos (dir:dirs) = do
    next <- move maze pos dir
    followPath maze next dirs

-- c
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = do
    rest <- go start dirs
    return (start : rest)
    where
        go :: Pos -> [Dir] -> Maybe [Pos]
        go pos [] = Just []
        go pos (dir:dirs) = do
            next <- move maze pos dir
            rest <- go next dirs
            return (next : rest)

-- task 2 decoding a message
type Key = Map Char Char

decryptChar :: Key -> Char -> Maybe Char
decryptChar key c = Map.lookup c key

decrypt :: Key -> String -> Maybe String
decrypt key text = traverse (decryptChar key) text

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key wordsList = traverse (decrypt key) wordsList

-- task 3 seating arrangements
type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings [] _ = [[]]
seatings guests conflicts = do
    seating <- permutations guests
    guard (validSeating conflicts seating)
    return seating

validSeating :: [Conflict] -> [Guest] -> Bool
validSeating conflicts guests = 
    all (not . hasConflict conflicts) (neighbourPairs guests)

neighbourPairs :: [Guest] -> [(Guest, Guest)]
neighbourPairs [] = []
neighbourPairs guests = zip guests (tail guests ++ [head guests])

hasConflict :: [Conflict] -> (Guest, Guest) -> Bool
hasConflict conflicts (a, b) = 
    (a, b) `elem` conflicts || (b, a) `elem` conflicts

-- task 4 result monad with warings
data Result a  = Failure String | Success a [String]
    deriving Show

-- a
instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success x warnings) = Success (f x) warnings

instance Applicative Result where
    pure x = Success x []
    Failure msg <*> _ = Failure msg
    _ <*> Failure msg = Failure msg
    Success f warnings1 <*> Success x warnings2 = 
        Success (f x) (warnings1 ++ warnings2)

instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success x warnings1 >>= f = 
        case f x of
            Failure msg -> Failure msg
            Success y warnings2 -> Success y (warnings1 ++ warnings2)


-- b
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure msg = Failure msg

-- c
validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure "age cannot be negative"
    | age > 150 = do
        warn "age is above 150"
        return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges ages = mapM validateAge ages


-- task 5 evaluator with simplification log
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
    deriving Show

logRule :: String -> Expr -> Writer [String] Expr
logRule msg expr = do
    tell [msg]
    return expr

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)

simplify (Add e1 e2) = do
    s1 <- simplify e1
    s2 <- simplify e2
    case (s1, s2) of
        (Lit 0, e) -> logRule "add identity: 0 + e -> e" e
        (e, Lit 0) -> logRule "add identity: e + 0 -> e" e
        (Lit a, Lit b) -> logRule "constant folding: a + b" (Lit (a + b))
        _ -> return (Add s1 s2)

simplify (Mul e1 e2) = do
    s1 <- simplify e1
    s2 <- simplify e2
    case (s1, s2) of
        (Lit 0, _) -> logRule "zero absorption: 0 * e -> 0" (Lit 0)
        (_, Lit 0) -> logRule "zero absorption: e * 0 -> 0" (Lit 0)
        (Lit 1, e) -> logRule "mul identity: 1 * e -> e" e
        (e, Lit 1) -> logRule "mul identity: e * 1 -> e" e
        (Lit a, Lit b) -> logRule "constant folding: a * b" (Lit (a*b))
        _ -> return (Mul s1 s2)

simplify (Neg e) = do
    s <- simplify e
    case s of
        Neg inner -> logRule "double negation: -(-e) -> e" inner
        _ -> return (Neg s)
        