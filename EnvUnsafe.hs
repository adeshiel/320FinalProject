module EnvUnsafe where

import Control.Monad(ap)

--This monad will form the plumbing for the evaluation function

data Unsafe a = Error String | Ok a deriving (Show, Eq)

data EnvUnsafe e a = EnvUnsafe (e -> Unsafe a )

-- function that just runs the function contained in EnvUnsafe
r ::  (EnvUnsafe e a) -> e -> Unsafe a
r (EnvUnsafe eu) e = eu e

-- a way to easily return an error (for instance in do notation)
err :: String -> EnvUnsafe e a
err s = EnvUnsafe $ \ _ -> Error s


-- a way to easily get the entire environment (for instance in do notation)
getEnv :: EnvUnsafe e e
getEnv = EnvUnsafe $ \ env -> Ok env


instance Functor (EnvUnsafe e) where
  -- fmap :: (a -> b) -> EnvUnsafe a -> EnvUnsafe 
  -- BARC
  fmap f (EnvUnsafe eu) = EnvUnsafe (\x -> case eu x of Error str -> Error str
                                                        Ok a -> Ok (f a))
  -- make sure your implemenation follows the functor laws

--ignore this for now
instance Applicative (EnvUnsafe e) where
  pure = return
  (<*>) = ap

instance Monad (EnvUnsafe e) where
  --return :: a -> EnvUnsafe a
  -- BARC
  return a = EnvUnsafe $ \_ -> Ok a

  --(>>=) :: EnvUnsafe a -> (a -> EnvUnsafe b) -> EnvUnsafe b
  -- BARC
  (EnvUnsafe eu) >>= f = EnvUnsafe (\x -> case (eu x) of Error str -> Error str
                                                         Ok a -> (r (f a) x))

  -- make sure your implementation follows the Monad laws

-- technical note: this could be done more succinctly with monad transformers, but it is also good practice to do it by hand