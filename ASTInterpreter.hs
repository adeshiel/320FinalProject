module ASTInterpreter where

import Prelude hiding (lookup)
import Data.Map(Map, lookup, insert, empty, fromList)  -- for State

import EnvUnsafe


data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast

         | Nil
         | Cons Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast

        --  deriving (Eq,Show) -- helpful to use this during testing
         deriving Eq

data Val = I Integer | B Bool
        | Ls [Val]
        | Fun (Val -> Unsafe Val) -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
 show (I i) = show i
 show (B b) = show b
 show (Ls ls) = show ls
 show (Fun _) = "->" -- no good way to show a function

-- BARC
stdLib = fromList [("tail", Fun $ \ v -> case v of Ls (_:ls)-> Ok $ Ls ls
                                                  _ -> Error "not an non empty list"),
                  ("head", Fun $ \v -> case v of Ls (l:ls) -> Ok l
                                                 _ -> Error "not something"),
                  ("len", Fun $ \v -> case v of Ls lst -> Ok $ I $ foldr (\_ n -> 1 + n) 0 lst
                                                _ -> Error "not something")]
-- stdLib = undefined



type Env = Map String Val


eval :: Ast -> EnvUnsafe Env Val
eval (ValBool a) = return $ B $ a
eval (And a b) = do n <- evalBool a
                   m <- evalBool b
                   return $ B $ n && m
eval (Or a b) = do n <- evalBool a
                  m <- evalBool b
                  return $ B $ n || m
eval (Not a) = do n <- evalBool a
                 return $ B $ not n

eval (ValInt a) = return $ I $ a
eval (Plus a b) = do n <- evalInt a
                    m <- evalInt b
                    return $ I $ n + m
eval (Minus a b) = do n <- evalInt a
                     m <- evalInt b
                     return $ I $ n - m
eval (Mult a b) = do n <- evalInt a
                    m <- evalInt b
                    return $ I $ n * m
eval (Div a b) = do n <- evalInt a
                   m <- evalInt b
                   case m of 0 -> err "fuck me"
                             i -> return $ I $ n `div` i
                   -- return $ I $ n `div` m

eval (Nil) = return $ Ls []
eval (Cons a b) = do n <- eval a
                    m <- evalList b
                    return $ Ls (n:m)

eval (If a b c) = do a' <- evalBool a
                    b' <- eval b
                    c' <- eval c
                    if a' == True
                    then return b'
                    else return c'
eval (Let var val bod) = eval $ App (Lam var bod) val

 -- do val' <- eval val
 --                           withVal var val' (eval bod)
eval (Var str) = valOf str
-- eval (Var str) = do env <- getEnv
--                     valOf str
eval (Lam var body) = do env <- getEnv
                        return $ Fun $ \v -> r (eval body) (insert var v env)
eval (App f a) = do a' <- eval a
                   f' <- evalFun f -- f' is type sign val -> unsafe val
                   case f' a' of Ok a -> return a
                                 Error s -> err s
