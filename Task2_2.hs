module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl x y []     = y                  
foldl x y (z:zs) = foldl x (x y z) zs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr x y []     = y 
foldr x y (z:zs) = x z (foldr x y zs) 

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr x y = case x y of
    Just (z, y') -> z : unfoldr x y'
    Nothing -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x y -> case x of 
                                Just x-> x:y
                                Nothing -> y) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal mat = reverse $ snd $ foldl f (0, []) mat 
    where f (l, result) lst = if length lst <= l then (l + 1, result) else (l + 1, lst !! l: result)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot con lst = foldr f [] lst
    where f x res = if con x then res else (x: res)

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e lst = foldl(\res x -> if res then True else x == e)  False lst

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step =  unfoldr (\from -> if from >= to then Nothing else Just (from, from+step)) from


-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append x y = foldr (:) y x

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\x ->if null x then Nothing else Just $ split' n x) lst
       where split' 0 y     = ([], y)
             split' _ []    = ([], [])
             split' n (y:ys)
                | n < 0     = ([], (y:ys))
                | otherwise = ((y:a), b)
                 where (a, b) = split' (n - 1) ys
