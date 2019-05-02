module Eval where

import Data.Map (Map)
import qualified Data.Map as Map

import Ast
import HelpShow
import EnvUnsafe

-- the goal of the program is to return a value
data Val = I Integer | F Float | B Bool | C Char | S [Char]
         | Ls [Val]
         | Fun (Val -> (Unsafe Val, [String])) -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show (I i) = show i
  show (F f) = show f
  show (B b) = show b
  show (C c) = show c
  show (S s) = show s
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function

instance Eq Val where
  I x == I y = x == y
  F x == F y = x == y
  B x == B y = x == y
  C x == C y = x == y
  S x == S y = x == y
  Ls x == Ls y = x == y
  _ == _ = False


stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> (Ok (Ls ls), [])
                                   _         -> (Error "can only call tail on a non empty list", [])),
   ("head", Fun $ \ v -> case v of Ls (x:_) -> (Ok x, [])
                                   _        -> (Error "can only call head on a non empty list", [])),
   ("len", Fun $ \ v -> case v of Ls x -> (Ok (I $ len x), [])
                                  _    -> (Error "can only call len on a non empty list", []))]
len [] = 0
len (x:xs) = 1 + (len xs)
-- helper function that runs with a standard library of functions: head, tail ...
run :: Ast -> (Unsafe Val, [String])
run a = runEnvUnsafe (eval a) stdLib



type Env = Map String Val


eval :: Ast -> EnvUnsafe Env Val
eval (ValBool x) = return $ B x
eval (And x y) =
  do x' <- evalBool x
     y' <- evalBool y
     return $ B $ x' && y'
eval (Or x y) =
  do x' <- evalBool x
     y' <- evalBool y
     return $ B $ x' || y'
eval (Not x) =
  do x' <- evalBool x
     return $ B $ not x'
eval (ValInt x) = return $ I x
eval (ValFlo f) = return $ F f
eval (Plus x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
                I i' -> return $ I $ i + i'
                _ -> err "not same type"
       F f -> case y' of
                F f' -> return $ F $ f + f'
                _ -> err "not same type"
       _ -> err "not a number"
eval (Minus x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
                I i' -> return $ I $ i - i'
                _ -> err "not same type"
       F f -> case y' of
                F f' -> return $ F $ f - f'
                _ -> err "not same type"
       _ -> err "not a number"
eval (Mult x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
                I i' -> return $ I $ i * i'
                _ -> err "not same type"
       F f -> case y' of
                F f' -> return $ F $ f * f'
                _ -> err "not same type"
       _ -> err "not a number"
eval (Div x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
         I 0 -> err "divied by 0"
         I i' -> return $ I $ i `div` i'
         _ -> err "not same type"
       F f -> case y' of
         F 0 -> err "divied by 0"
         F f' -> return $ F $ f / f'
         _ -> err "not same type"
       _ -> err "not a number"
eval (Exp x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
                I i' -> return $ I $ i ^ i'
                _ -> err "not same type"
       F f -> case y' of
                F f' -> return $ F $ f ** f'
                _ -> err "not same type"
       _ -> err "not a number"
eval (Mod x y) =
  do x' <- evalInt x
     y' <- evalInt y
     return $ I $ x' `mod` y'
eval (Lt x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
                I i' -> return $ B $ i < i'
                _ -> err "not same type"
       F f -> case y' of
                F f' -> return $ B $ f < f'
                _ -> err "not same type"
       _ -> err "not a number"
eval (Le x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
                I i' -> return $ B $ i <= i'
                _ -> err "not same type"
       F f -> case y' of
                F f' -> return $ B $ f <= f'
                _ -> err "not same type"
       _ -> err "not a number"
eval (Gt x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
                I i' -> return $ B $ i > i'
                _ -> err "not same type"
       F f -> case y' of
                F f' -> return $ B $ f > f'
                _ -> err "not same type"
       _ -> err "not a number"
eval (Ge x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       I i -> case y' of
                I i' -> return $ B $ i >= i'
                _ -> err "not same type"
       F f -> case y' of
                F f' -> return $ B $ f >= f'
                _ -> err "not same type"
       _ -> err "not a number"
--
eval (Eq x y) =
  do x' <- eval x
     y' <- eval y
     case x' of
       Fun f -> err "cannot compare functions"
       _ -> case y' of
              Fun f' -> err "cannot compare functions"
              _ -> return $ B $ x' == y'
eval (UEq x y) =
  do r <- eval (Eq x y)
     case r of
       B True -> return $ B False
       B False -> return $ B True
       _ -> return r
eval Nil = return $ Ls []
eval (Cons x y) =
  do x' <- eval x
     y' <- evalLs y
     return $ Ls $ x' : y'
eval (If x y z) =
  do x' <- evalBool x
     y' <- eval y
     z' <- eval z
     if x' then return y' else return z'
eval (Let s x y) =
  do env <- getEnv
     x' <- eval x
     case runEnvUnsafe (eval y) (Map.insert s x' env) of
       (Ok a, l) -> return a
       (Error s, l') -> err s
eval (Var s) =
  do env <- getEnv
     case Map.lookup s env of
       Just a -> return a
       _ -> err "variable not in the scope"
eval (Lam s x) =
  do env <- getEnv
     return $ Fun $ \v -> runEnvUnsafe (eval x) (Map.insert s v env)
eval (App x y) =
  do x' <- evalFun x
     y' <- eval y
     case x' y' of
       (Ok a, l) -> return a
       (Error s, l') -> err s


-- helper functions that take care of type issues (use a "Error" when things have the wron type
evalInt :: Ast -> EnvUnsafe Env Integer
evalInt a =
  do a' <- eval a
     case a' of
       I i -> return i
       _ -> err "not an Integer"

evalBool :: Ast -> EnvUnsafe Env Bool
evalBool a =
  do a' <- eval a
     case a' of
       B i -> return i
       _ -> err "not a Bool"

evalFun :: Ast -> EnvUnsafe Env (Val -> (Unsafe Val, [String]))
evalFun a =
  do a' <- eval a
     case a' of
       Fun i -> return i
       _ -> err "not a Function"

evalLs :: Ast -> EnvUnsafe Env [Val]
evalLs a =
  do a' <- eval a
     case a' of
       Ls i -> return i
       _ -> err "not a List"
