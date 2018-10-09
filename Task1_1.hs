module Task1_1 where

import Todo(todo)
data Operator = Plus 
   | Minus 
   | Multi 
   deriving(Show,Eq)
data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, op :: Operator } -- бинарная операция
         -- | Expr { val :: Term, op :: Operator }
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Minus
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Multi
infixl 6 |+|,|-|
infixl 7 |*|
-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar _ _ t@IntConstant{} = t 
replaceVar vars replacement Variable{ varName = s' } | vars==s' =  replacement
                                                        | otherwise = Variable s'
replaceVar varName replacement BinaryTerm{lhv=l,rhv=r,op=o} = BinaryTerm (replaceVar varName replacement l) (replaceVar varName replacement r) o
-- Посчитать значение выражения `Term`
-- если оно состоит только из констант

evaluate :: Term -> Term
evaluate IntConstant{intValue=x} =IntConstant x
evaluate Variable{varName=x} =Variable x
evaluate BinaryTerm {lhv=IntConstant 0,rhv=x, op=Plus} =evaluate x
evaluate BinaryTerm {lhv=x,rhv=IntConstant 0, op=Plus} =evaluate x
evaluate BinaryTerm {lhv=IntConstant 0,rhv=x, op=Minus} =evaluate x
evaluate BinaryTerm {lhv=x,rhv=IntConstant 0, op=Minus} =evaluate x
evaluate BinaryTerm {lhv=IntConstant 0,rhv=x, op=Multi} = IntConstant 0
evaluate BinaryTerm {lhv=x,rhv=IntConstant 0, op=Multi} = IntConstant 0
evaluate BinaryTerm {lhv=IntConstant 1,rhv=x, op=Multi} =evaluate x
evaluate BinaryTerm {lhv=x,rhv=IntConstant 1, op=Multi} =evaluate x
evaluate BinaryTerm {lhv=IntConstant l,rhv=IntConstant r, op=Plus}=IntConstant (l+r)
evaluate BinaryTerm {lhv=IntConstant l,rhv=IntConstant r, op=Minus}=IntConstant (l-r)
evaluate BinaryTerm {lhv=IntConstant l,rhv=IntConstant r, op=Multi}=IntConstant (l*r)
evaluate BinaryTerm {lhv=l,rhv=Variable r, op=o}=(BinaryTerm (evaluate l) (Variable r) o)
evaluate BinaryTerm {lhv=Variable l,rhv=r, op=o}=(BinaryTerm (Variable l) (evaluate r) o)
evaluate BinaryTerm {lhv=l,rhv=IntConstant r, op=o}=(BinaryTerm (evaluate l) (IntConstant r) o)
evaluate BinaryTerm {lhv=IntConstant l,rhv=r, op=o}=(BinaryTerm (IntConstant l) (evaluate r) o)
evaluate BinaryTerm {lhv=l, rhv=r, op=o}=evaluate (BinaryTerm (evaluate l) (evaluate r) o)
