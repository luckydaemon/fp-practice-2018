module Task1_1 where

import Todo (todo)
data Operator = Plus 
                | Minus 
                | Multi 
                deriving (Show, Eq)
data Term = IntConstant {intValue :: Int}           -- числовая константа
            | Variable{varName :: String}          -- переменная
            | BinaryTerm{lhv :: Term, rhv :: Term, op :: Operator} -- бинарная операция
            deriving (Show, Eq)

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
replaceVar vars replacement Variable {varName = s'} | vars == s' =  replacement
                                                    | otherwise = Variable s'
replaceVar varName replacement BinaryTerm{lhv = l,rhv = r,op = o} 
                    = BinaryTerm (replaceVar varName replacement l) (replaceVar varName replacement r) o
-- Посчитать значение выражения `Term`
-- если оно состоит только из констант

evaluate :: Term -> Term
evaluate IntConstant{intValue = x} = IntConstant x
evaluate Variable{varName = x} = Variable x
evaluate (BinaryTerm (IntConstant l) r o) | ((l == 0) && ((o == Plus) || (o == Minus))) = evaluate r
                                          | ((l == 0) && (o == Multi)) = IntConstant 0
                                          | ((l == 1) && (o == Multi)) = evaluate r
evaluate (BinaryTerm l (IntConstant r) o) | ((r == 0) && ((o == Plus) || (o == Minus))) = evaluate l
                                          | ((r == 0) && (o == Multi)) = IntConstant 0
                                          | ((r == 1) && (o == Multi)) = evaluate l
evaluate (BinaryTerm (IntConstant l) (IntConstant r) o) | (o == Plus) = IntConstant (l + r)
                                                        | (o == Minus) = IntConstant (l - r)
                                                        | (o == Multi) = IntConstant (l * r)
evaluate (BinaryTerm l r o) = BinaryTerm (evaluate l) (evaluate r) o
