module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)


-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Empty
                 | Leaf Integer  v
                 | Node Integer  v (TreeMap v) (TreeMap v) 
                  deriving(Show,Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Empty t = False
contains (Leaf t v) k   | (t == k) = True
                        | otherwise = False
contains (Node t _ l r) k  | (t == k) = True
                           | (t > k) = contains l k
                           | (t < k) = contains r k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k Empty = error "Nothing"
lookup k (Leaf key value) | (key==k) =value 
                    |otherwise =error "Nothing"
lookup k (Node key value l r)| (k==key) = value
                             | (k<key)=lookup k l
                             | (k>key)=lookup k r
-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Empty = Leaf k v
insert (k, v) (Leaf key value) | (k == key) = Leaf key value
                               | (k > key) = Node key value Empty (Leaf key value) 
                               | (k > key) = Node key value (Leaf key value) Empty
insert (k, v) (Node key value l r) | (k == key) = Node key value l r
                                   | (k < key) = Node key value (insert (k,v) l) r
                                   | (k > key) = Node key value l (insert(k,v) r)
-- Удаление элемента по ключу
putInLeft x Empty = x
putInLeft x (Leaf key value) = Node key value x Empty
putInLeft x (Node key value l r)= Node key value (putInLeft x l) r

remove :: Integer -> TreeMap v -> TreeMap v
remove i Empty = error "Nothing to delete"
remove i (Leaf key value) | (i == key) = Empty
                          | otherwise = Leaf key value
remove i (Node key value l r)  | (i == key) = putInLeft r l
                               | (i < key) = Node key value (remove i l) r
                               | (i > key) = Node key value l (remove i r)

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i Empty = error "Nothing"
nearestLE i (Leaf key value) | (i >= key) = (key, value)
                             | otherwise = error "Nothing found"
nearestLE i (Node key value l r) | (i == key) = (key,value)
                                 | (i < key) = case l of (Leaf k v) | (k <= i) -> (k,v)
                                                                    | otherwise -> error "Nothing found"
                                 | (i < key) = case l of (Node k v nl nr) | (i > k) -> case nr of (Node sk sv sl sr)  -> nearestLE i nr
                                                                          | (i > k) -> case nr of Empty -> (k,v)
                                                                          | (i > k) -> case nr of (Node sk sv sl sr) | (sk > i) -> (k,v)
                                                                                                                     | otherwise -> nearestLE i nr
                                 | (i > key) = case l of (Node k v nl nr) | (i < k) -> nearestLE i l    
                                 | (i > key) = case r of Empty ->(key,value)
                                 | (i > key) = case l of Empty -> error "Nothing"
                                 | otherwise = case r of (Leaf k v) | (k < i) -> nearestLE i r
                                                                    | otherwise -> (key,value)
                                 | otherwise = case r of (Node k v nl nr) | (k < i) -> nearestLE i r
                                                                    | otherwise -> (key,value)         
-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Empty lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Empty = []
listFromTree (Leaf key value) = [(key,value)]
listFromTree (Node key value l r) = (listFromTree l) ++ [(key,value)] ++ (listFromTree r)

-- Поиск k-той порядковой статистики дерева 
treeSize :: TreeMap v -> Integer
treeSize Empty                 = 0
treeSize (Leaf key value) = 1
treeSize (Node _ _ l r) = (treeSize l) + 1 + (treeSize r)


kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i Empty = error "Nothing"
kMean i (Leaf key value) | (i == 0) = (key,value)
                         | otherwise = error "Nothing"
kMEan i (Node key value l r) | (treeSize l == i) = (key,value)
                             | (treeSize l > i) = kMean i l
                             | (treeSize l < i) = kMean (i - (treeSize l) - 1) r
