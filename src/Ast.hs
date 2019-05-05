module Ast where

import HelpShow


-- Here is the abstract syntax tree for our language

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer | ValFlo Float | FDiv Ast Ast | FExp Ast Ast
         | UnaryMinus Ast
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast | Exp Ast Ast | Mod Ast Ast
         | Lt Ast Ast | Le Ast Ast | Gt Ast Ast | Ge Ast Ast | Eq Ast Ast | NEq Ast Ast

         | ValChar Char | ValStr [Char]

         | Nil
         | Cons Ast Ast
         | CPlus Ast Ast | Index Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Print Ast
         | Sep Ast Ast
         | Lam String Ast
         | App Ast Ast
           deriving (Eq,Show) -- helpful to use this during testing
--         deriving Eq

--instance Show Ast where
--  show ast = showPretty ast 0



-- This is helpful for testing and debugging
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValFlo f) = "(" ++ show f ++ ")"
showFullyParen (UnaryMinus u) = "(-" ++ (showFullyParen u) ++ ")"
showFullyParen (ValChar c) = "(" ++ [c] ++ ")"
showFullyParen (ValStr s) = "(" ++ s ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Eq l r) = "(" ++ (showFullyParen l) ++ " == " ++ (showFullyParen r) ++ ")"
showFullyParen (NEq l r) = "(" ++ (showFullyParen l) ++ " /= " ++ (showFullyParen r) ++ ")"
showFullyParen (Lt l r) = "(" ++ (showFullyParen l) ++ " < " ++ (showFullyParen r) ++ ")"
showFullyParen (Le l r) = "(" ++ (showFullyParen l) ++ " <= " ++ (showFullyParen r) ++ ")"
showFullyParen (Gt l r) = "(" ++ (showFullyParen l) ++ " > " ++ (showFullyParen r) ++ ")"
showFullyParen (Ge l r) = "(" ++ (showFullyParen l) ++ " >= " ++ (showFullyParen r) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (FDiv l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (Exp l r) = "(" ++ (showFullyParen l) ++ " ** " ++ (showFullyParen r) ++ ")"
showFullyParen (FExp l r) = "(" ++ (showFullyParen l) ++ " ^ " ++ (showFullyParen r) ++ ")"
showFullyParen (Mod l r) = "(" ++ (showFullyParen l) ++ " % " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Print s) = "( print " ++ (showFullyParen s) ++ ")"
showFullyParen (Sep l r) = "(" ++ (showFullyParen l) ++ " ; " ++ (showFullyParen r) ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen (CPlus h t) = "(" ++ (showFullyParen h)  ++ " ++ " ++ (showFullyParen t) ++ ")"
showFullyParen (Index h t) = "(" ++ (showFullyParen h)  ++ " !! " ++ (showFullyParen t) ++ ")"

showFullyParen Nil = "( [] )"


-- provide a nice show with minimal parentheses, for testing an documentation



--the bigger the number the more tight the biding
showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _      =  if i < 0
                                then  "(" ++ show i ++ ")"
                                else show i
showPretty (ValFlo f) _      =  if f < 0
                                then "(" ++ show f ++ ")"
                                else show f
showPretty (ValBool True) _  =  "true"
showPretty (ValBool False) _ = "false"
showPretty (ValChar c) _ = show c
showPretty (ValStr s) _ = s
showPretty Nil _ = "[]"
showPretty (Var s) _ = s
showPretty (Lam v bod) i     = parenthesize 1 i  $ "\\ " ++ v ++ " -> "        ++ (showPretty bod 100)
showPretty (Let v a bod) i   = parenthesize 1 i  $  "let " ++ v ++ " = "       ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i      = parenthesize 1 i  $  "if " ++ (showPretty b 1)  ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)
showPretty (Sep l r) i       = parenthesize 2 i  $ (showPretty l 2)  ++ " ; "  ++ (showPretty r 3)
showPretty (App l r) i       = parenthesize 4 i  $ (showPretty l 4)  ++ " "    ++ (showPretty r 5)
showPretty (Or l r) i        = parenthesize 6 i  $ (showPretty l 6)  ++ " || " ++ (showPretty r 7)
showPretty (And l r) i       = parenthesize 8 i  $ (showPretty l 8)  ++ " && " ++ (showPretty r 9)
showPretty (Eq l r) i        = parenthesize 10 i  $ (showPretty l 11) ++ " == " ++ (showPretty r 11)
showPretty (NEq l r) i       = parenthesize 10 i  $ (showPretty l 11) ++ " /= " ++ (showPretty r 11)
showPretty (Lt l r) i        = parenthesize 10 i  $ (showPretty l 11) ++ " < " ++ (showPretty r 11)
showPretty (Le l r) i        = parenthesize 10 i  $ (showPretty l 11) ++ " <= " ++ (showPretty r 11)
showPretty (Ge l r) i        = parenthesize 10 i  $ (showPretty l 11) ++ " >= " ++ (showPretty r 11)
showPretty (Gt l r) i        = parenthesize 10 i  $ (showPretty l 11) ++ " > " ++ (showPretty r 11)
showPretty (Cons l r) i      = parenthesize 12 i  $ (showPretty l 13)  ++ " : "  ++ (showPretty r 12)
showPretty (CPlus l r) i     = parenthesize 12 i  $ (showPretty l 13)  ++ " ++ "  ++ (showPretty r 12)
showPretty (Minus l r) i     = parenthesize 14 i $ (showPretty l 14) ++ " - "  ++ (showPretty r 15)
showPretty (Plus l r) i      = parenthesize 14 i $ (showPretty l 14) ++ " + "  ++ (showPretty r 15)
showPretty (Mult l r) i      = parenthesize 16 i $ (showPretty l 16) ++ " * "  ++ (showPretty r 17)
showPretty (FDiv l r) i      = parenthesize 16 i $ (showPretty l 16) ++ " / "  ++ (showPretty r 17)
showPretty (Div l r) i       = parenthesize 16 i $ (showPretty l 16) ++ " // "  ++ (showPretty r 17)
showPretty (Mod l r) i       = parenthesize 16 i $ (showPretty l 16) ++ " % "  ++ (showPretty r 17)
showPretty (FExp l r) i      = parenthesize 18 i $ (showPretty l 19) ++ " ^ "  ++ (showPretty r 18)
showPretty (Exp l r) i       = parenthesize 18 i $ (showPretty l 19) ++ " ** "  ++ (showPretty r 18)
showPretty (Index l r) i     = parenthesize 20 i $ (showPretty l 20) ++ " !! "  ++ (showPretty r 21)
showPretty (Not l ) i        = parenthesize 16 i $  " ! " ++ (showPretty l 16)
showPretty (UnaryMinus l) i  = parenthesize 16 i $  "( - " ++ (showPretty l 16) ++ " )"
showPretty (Print l ) i      = parenthesize 16 i $  " print " ++ (showPretty l 16)
