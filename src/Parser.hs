module Parser where

import Ast
import ParserMonad
import Data.Char

-- | parser for the language
keywords = ["if","then","else", "let", "in", "true","false"]

parens :: Parser Ast
parens = do com $ token $ literal "("
            ast <- parser
            com $ token $ literal ")"
            return ast

vars :: Parser Ast
vars = do s <- com $ token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

chars :: Parser Ast
chars = do com $ token $ literal "\'"
           c <- com $ token $ item
           com $ token $ literal "\'"
           return $ ValChar c

strs :: Parser Ast
strs = do com $ token $ literal "\""
          s <- com $ token $ varParser
          com $ token $ literal "\""
          return $ ValStr s

ints :: Parser Ast
ints = token intParser
                `mapParser` (\ i -> ValInt i)

bools :: Parser Ast
bools = do s <- com $ token ((literal "true") <||> (literal "false"))
           case s of
             Left _ -> return $ ValBool True
             Right _ -> return $ ValBool False

list :: Parser Ast
list = do com $ token (literal "[")
          s <- com $ token $ list' <||> (literal "]")
          case s of
            Left s' -> return s'
            Right _ -> return Nil

list' :: Parser Ast
list' = do x <- parser
           com $ token (literal ",")
           xs <- com $ list' <||> (parser +++ (token $ literal "]"))
           case xs of
             Left xs' -> return $ Cons x xs'
             Right (xs'', _) -> return $ Cons x (Cons xs'' Nil)


ifParser :: Parser Ast
ifParser = do com $ token (literal "if")
              a <- parser
              com $ token (literal "then")
              b <- parser
              com $ token (literal "else")
              c <- parser
              return $ If a b c

letParser :: Parser Ast
letParser = do com $ token (literal "let")
               s <- com $ varParser
               com $ token (literal "=")
               a <- parser
               com $ token (literal "in")
               b <- parser
               return $ Let s a b

lambdaParser :: Parser Ast
lambdaParser = do com $ token (literal "\\")
                  s <- com $ varParser
                  com $ token (literal "->")
                  x <- parser
                  return $ Lam s x

atoms:: Parser Ast
atoms =  com ints <|> bools <|> list <|> parens <|> ifParser <|> letParser <|> lambdaParser <|> strs <|> chars <|> vars

printsOrunarysOrnots :: Parser Ast
printsOrunarysOrnots = do a <- com $ token $ (literal "print") <||> (literal "-") <||> (literal "!")
                          x <- com printsOrunarysOrnotsOratoms
                          case a of
                            Left (Left _) -> return $ Print x
                            Left (Right _) -> return $ UnaryMinus x
                            Right _ -> return $ Not x

printsOrunarysOrnotsOratoms :: Parser Ast
printsOrunarysOrnotsOratoms = printsOrunarysOrnots <|> atoms

indexes :: Parser Ast
indexes = withInfix printsOrunarysOrnotsOratoms [("!!", Index)]

expsOrfexps :: Parser Ast
expsOrfexps = do l <- indexes
                 x <- com $ token $ (literal "**") <||> (literal "^")
                 r <- com expsOrfexpsOrindexes
                 case x of
                   Left _ -> return $ Exp l r
                   Right _ -> return $ FExp l r

expsOrfexpsOrindexes :: Parser Ast
expsOrfexpsOrindexes = expsOrfexps <|> indexes

multDivModExpr :: Parser Ast
multDivModExpr = withInfix expsOrfexpsOrindexes [("*", Mult), ("/", FDiv), ("//", Div), ("%", Mod)]

addSubExpr :: Parser Ast
addSubExpr = withInfix multDivModExpr [("+", Plus), ("-", Minus)]

consOrcplus :: Parser Ast
consOrcplus = do l <- addSubExpr
                 x <- com $ token $ (literal ":") <||> (literal "++")
                 r <- com consOrcplusOraddSubExpr
                 case x of
                   Left _ -> return $ Cons l r
                   Right _ -> return $ CPlus l r

consOrcplusOraddSubExpr :: Parser Ast
consOrcplusOraddSubExpr = consOrcplus <|> addSubExpr

comps :: Parser Ast
comps = boolInfix consOrcplusOraddSubExpr [("==", Eq), ("/=", NEq), ("<=", Le), ("<", Lt), (">=", Ge), (">", Gt)]

andExpr :: Parser Ast
andExpr = withInfix comps [("&&", And)]

orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

apps :: Parser Ast
apps = withInfix orExpr [("", App)]

seps :: Parser Ast
seps = do l <- apps
          com $ token (literal ";")
          r <- com appsOrseps
          return $ Sep l r

appsOrseps :: Parser Ast
appsOrseps = seps <|> apps

parser :: Parser Ast
parser = com appsOrseps
