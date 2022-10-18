
-- PART ONE --
-- 1) --

data List a = Nil | Cons a (List a)

--lengthList (Cons _ xs) = 1 + lengthList xs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Monad List where 
        return a = Cons a Nil
        Nil >>= _ = Nil 
        Cons a list >>= f = (f a) `append ` (list >>= f)


-- FROM CLASS
instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons a b) =  Cons

instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil

    <*> :: List a -> List b -> List b
    Nil <*> l = Nil
    l <*> Nil = Nil
    (Cons f f fs) <*> l = f <$> l fs <$> l

instance Monad List where 
        return = pure
        Nil >>= _ = Nil 
        Cons a list >>= f = (f a) `append ` (list >>= f)

-- 2) --

-- proofs --

-- associativity law (I had trouble proving this)

-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

-- f >=> g =  \x -> f x >>= g
-- f >=> g =  (f a) (list >>= f)
-- 

-- left identity 

-- return a >>= f = f a

-- return a = List a            -- creating a list type
-- return a = Cons a (List a)   -- separating the original value
-- return a = Cons a Nil        -- as there is only one value 

-- right identity 

-- m >>= return = m

-- Cons m Nil = m
-- Cons m = m       -- since there is nothing in Nil
-- m = m            -- since there is only one value in a Cons



-- PART TWO --

--We met Functor and Applicative in the process of learning about Monad. It might seem pointless 
-- to have three 
-- classes when one would do. But in fact not everything that can be an instance of Functor can 
-- be an instance of Applicative, and not everything that can be an instance of Applicative can be a Monad. 
-- Yet they could still be useful.
--Tuples are built in, like lists. But they are not Monads. Here's a data type that represents 2-tuples. 


--Give an instance of Functor for Pair, and prove that the Functor laws hold for it.

--Attempt to give an instance of Applicative for Pair, notice that you can't. Why?


data Pair a b = Pair a b

-- 1) --
instance Functor Pair where
    fmap f (Pair x y) = Pair x (f y)


--- Identity

-- fmap id ≡ id

-- fmap P x y - this has no function applied so no need for fmap
-- P x y

-- Composition

-- fmap (f . g) ≡ fmap f . fmap g

-- fmap (f (P x y) . g (P x y)) 
-- fmap f (P x y) . fmap g (P x y)


-- 2) --

-- There is no way to turn a pair of pairs into a single pair without losing some of the data

