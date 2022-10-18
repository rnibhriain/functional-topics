--- CORRECTIONS PT 1 ---
{-type Log = [String]
data Writer a = Writer Log a
  deriving Show

runWriter :: Writer a -> (Log a)
runWriter (Writer l x) = (l, x)

instance Functor Writer where
  fmap f (Writer log val) = Writer log (f val)

instance Applicative Writer where
  pure x = Writer [] x  ORR pure =  Writer []
  (Writer l1 f) <*> (Writer l2 a) = Writer (l1++l2) f a

instance Monad Writer where
  return = pure
  m >>= k = let Writer l a = m
            Writer l' a' = ka
            in Writer (l++l') a'


tell :: String -> Writer ()
tell logMessage = Writer [logMessage] ()


--- CORRECTIONS PT 2 ---

data Writer2 l a = Writer [l] a

runWriter2 :: Monoid l => Writer2 l a -> ([l], a)
runWriter2 (Writer2 l x)  = (l, x)

instance 


-}


-- PART ONE --

newtype Writer a = Writer { runWriter :: ([String], a) } deriving Show

instance Functor Writer where
  fmap f (Writer (log, x)) = Writer (log, f x)

instance Applicative Writer where
  pure x = Writer ([], x)
  (<*>) (Writer (log1, fx)) (Writer (log2, x)) =
    Writer (mappend log1 log2, fx x)

instance Monad Writer where
  return = pure
  (>>=) (Writer (log1, x)) nextFn =
    let Writer (log2, y) = nextFn x
    in Writer (mappend log1 log2, y)

tell :: String -> Writer ()
tell log = Writer ([log], ())

--log :: String -> Writer [String] ()
--log msg = tell [msg]

example :: Writer Int
example = do
  tell "entry 1"
  tell "entry 2"
  return (1 + 1)



main :: IO ()
main = print example


-- PART TWO --

-- Have the log be a list of any type, which would allow the programmer to create a type for individual log entries
-- Have the log be of any type at all, letting the programmer create a type for the log overall.


-- there can't be concatenation with the different types in a list so if you have an Int then a String these can't be concatenated
-- if it isn't a list type there can't be concatenation either
