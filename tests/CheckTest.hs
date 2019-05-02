module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck

import Data.Set (Set)
import qualified Data.Set as Set
import Ast
import Check

-- provide tests that show your check works
--TODO: move the generator to a shared place

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- arbitrary
                                 b <- arbitrary
                                 node <- elements [ValInt i, ValBool b, Nil]
                                 return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
                                     str <- elements ["x","y","z"]
                                     ifast <- arbitrarySizedIf m
                                     node <- elements [And l r, Or l r, Not l,
                                                       Plus l r, Minus l r, Mult l r, Div l r,
                                                       Cons l r,
                                                       ifast,
                                                       Let str l r,
                                                       Lam str l,
                                                       App l r,
                                                       Var str
                                                      ]
                                     return node

-- it would be better if every branch were built like this so the balance would be maintained
arbitrarySizedIf ::  Int -> Gen Ast
arbitrarySizedIf m = do b <- arbitrarySizedAst (m `div` 3)
                        t <- arbitrarySizedAst (m `div` 3)
                        e <- arbitrarySizedAst (m `div` 3)
                        return $ If b t e

checkTest = testGroup "CheckTest"
  [
  testCase "reference in expression" $
    do
      assertBool "\\x -> x has no undefined var" $ (check (Lam "x" (Var "x"))) == Set.empty
  -- ...
  ]
