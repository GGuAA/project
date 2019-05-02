module Parser where

import Ast
import ParserMonad


-- | parser for the language
keywords = ["if","then","else", "let", "in", "true","false"]

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = token intParser
                `mapParser` (\ i -> ValInt i)

bools :: Parser Ast
bools = do s <- token ((literal "true") <||> (literal "false"))
           case s of
             Left _ -> return $ ValBool True
             Right _ -> return $ ValBool False

nils :: Parser Ast
nils = do token (literal "[]")
          return Nil

ifParser :: Parser Ast
ifParser = do token (literal "if")
              a <- parser
              token (literal "then")
              b <- parser
              token (literal "else")
              c <- parser
              return $ If a b c

letParser :: Parser Ast
letParser = do token (literal "let")
               s <- varParser
               token (literal "=")
               a <- parser
               token (literal "in")
               b <- parser
               return $ Let s a b

lambdaParser :: Parser Ast
lambdaParser = do token (literal "\\")
                  s <- varParser
                  token (literal "->")
                  x <- parser
                  return $ Lam s x

atoms:: Parser Ast
atoms =  ints <|> bools <|> nils <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars

nots :: Parser Ast
nots = do token (literal "!")
          b <- atomsOrnots
          return $ Not b

atomsOrnots :: Parser Ast
atomsOrnots = nots <|> atoms

multDivExpr :: Parser Ast
multDivExpr = withInfix atomsOrnots [("*", Mult), ("/", Div)]

addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus), ("-", Minus)]

andExpr :: Parser Ast
andExpr = withInfix addSubExpr [("&&", And)]

orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

cons :: Parser Ast
cons = do l <- orExpr
          token (literal ":")
          r <- orExprOrcons
          return $ Cons l r

orExprOrcons :: Parser Ast
orExprOrcons = cons <|> orExpr

apps :: Parser Ast
apps = withInfix orExprOrcons [("", App)]

parser :: Parser Ast
parser = apps
