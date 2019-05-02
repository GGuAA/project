module Check where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast

-- here you can preform static checks


-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors
data WarningMsg =
    UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name
  -- ...
  deriving (Show,Eq,Ord)

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check x | (freeVars x) == Set.empty = Set.empty
        | True = Set.map (\v -> UndefinedVarUse ("Variable Undefined: " ++ v)) (freeVars x)

freeVars :: Ast -> Set String
freeVars (And x y) = Set.union (freeVars x) (freeVars y)
freeVars (Or x y) = Set.union (freeVars x) (freeVars y)
freeVars (Not x) = freeVars x
freeVars (Plus x y) = Set.union (freeVars x) (freeVars y)
freeVars (Minus x y) = Set.union (freeVars x) (freeVars y)
freeVars (Mult x y) = Set.union (freeVars x) (freeVars y)
freeVars (Div x y) = Set.union (freeVars x) (freeVars y)
freeVars (Cons x y) = Set.union (freeVars x) (freeVars y)
freeVars (If x y z) = Set.union (Set.union (freeVars x) (freeVars y)) (freeVars z)
freeVars (Let s x y) = Set.union (Set.delete s (freeVars y)) (freeVars x)
freeVars (Var v) = Set.singleton v
freeVars (App x y) = Set.union (freeVars x) (freeVars y)
freeVars (Lam a x) = Set.delete a (freeVars x)
freeVars _ = Set.empty
