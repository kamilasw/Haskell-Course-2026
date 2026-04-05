
import Data.Monoid (Sum(..))
data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving Show

-- task 1 functor for sequence
instance Functor Sequence where
    fmap f Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append left right) = Append (fmap f left) (fmap f right)


-- task 2 foldable for sequence
instance Foldable Sequence where
    foldMap f Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append left right) =
        foldMap f left <> foldMap f right

seqToList :: Sequence a -> [a]
seqToList = foldMap (\x -> [x])



seqLength :: Sequence a -> Int
seqLength s = getSum (foldMap (\_ -> Sum 1) s)


-- task 3 semigroup and monoid for sequence
instance Semigroup (Sequence a) where
    Empty <> right = right
    left <> Empty = left
    left <> right = Append left right

instance Monoid (Sequence a) where
    mempty = Empty


-- task 4 tail recursion and sequence search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x seq = go [seq]
  where
    go [] = False
    go (s:stack) =
      case s of
        Empty -> go stack
        Single y -> 
            if x == y
                then True
                else go stack
        Append l r -> go (l : r : stack)


-- task 5 tail recursion and sequence flatten
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
  where
    go [] acc = reverse acc
    go (s:stack) acc =
      case s of
        Empty -> go stack acc
        Single x -> go stack (x : acc)
        Append l r -> go (l : r : stack) acc


-- task 6 tail recursion and reverse polish notation
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing

    go (TNum n : ts) stack = go ts (n : stack)

    go (TAdd : ts) (x:y:stack) = go ts ((y + x) : stack)
    go (TSub : ts) (x:y:stack) = go ts ((y - x) : stack)
    go (TMul : ts) (x:y:stack) = go ts ((y * x) : stack)
    go (TDiv : ts) (0:y:stack) = Nothing
    go (TDiv : ts) (x:y:stack) = go ts ((y `div` x) : stack)

    go (_ : _) _ = Nothing


-- task 7 expressing functions via foldr and foldl
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0


-- task 8  run-length encoding via folds
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x,1)]
    step x ((y,n):ys)
      | x == y    = (y, n + 1) : ys
      | otherwise = (x,1) : (y,n) : ys

decode :: [(a, Int)] -> [a]
decode = foldr (\(x,n) acc -> replicate n x ++ acc) []