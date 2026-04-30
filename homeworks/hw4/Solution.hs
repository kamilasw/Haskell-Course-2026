
newtype Reader r a = Reader { runReader :: r -> a }


-- task 1 functor applicative and monad instances

instance Functor (Reader r) where
    fmap f (Reader getA) =
        Reader getB
        where
            getB env = 
                f (getA env) 
        
        
instance Applicative (Reader r) where
    pure x = 
        Reader getX
        where
            getX env =
                x
    
    liftA2 f (Reader getA) (Reader getB) =
        Reader getC
        where
            getC env =
                f (getA env) (getB env)

instance Monad (Reader r) where
    (Reader getA) >>= makeReaderB =
        Reader getB
        where
            getB env = 
                let a = getA env
                    Reader getResult = makeReaderB a
                in getResult env
    

-- task 2 primittive operations

ask :: Reader r r
ask = 
    Reader getEnvironment 
    where
        getEnvironment env = 
            env

asks :: (r -> a) -> Reader r a
asks projection = 
    Reader getValue
    where
        getValue env = 
            projection env

local :: (r -> r) -> Reader r a -> Reader r a
local changeEnvironment reader = 
    Reader getValue
    where
        getValue env =
            runReader reader (changeEnvironment env)

-- task 3 banking sys

data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)



calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
    rate <- asks interestRate
    pure (round (fromIntegral (balance account) * rate))



applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    fee <- asks transactionFee
    pure account { balance = balance account - fee}


checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    minBalance <- asks minimumBalance
    pure (balance account >= minBalance)


processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    accountAfterFee <- applyTransactionFee account
    interest <- calculateInterest account
    hasMinimumBalance <- checkMinimumBalance account
    pure (accountAfterFee, interest, hasMinimumBalance)