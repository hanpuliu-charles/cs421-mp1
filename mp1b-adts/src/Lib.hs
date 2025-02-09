--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a 
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x y) = x : cons2list y

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp x) = x
-- recursive
eval (PlusExp []) = 0
eval( PlusExp (x:xs)) = eval x + eval (PlusExp xs)
-- Higher order functions
eval (MultExp []) = 1
eval( MultExp  x) = foldl (*) 1 (map eval x) 
--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' x = foldr (Cons) Nil x


--- ### BinTree

-- BinTree

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree = undefined

--- ### SimpVal

-- SimpVal

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp = undefined
