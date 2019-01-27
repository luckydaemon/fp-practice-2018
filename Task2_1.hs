module Task2_1 where

import Todo (todo)
import Prelude hiding (lookup)


-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Empty
                 | Leaf Integer  v
                 | Node Integer  v (TreeMap v) (TreeMap v) 
                  deriving (Show,Eq)

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
lookup k (Node key value l r)| (k == key) = value
                             | (k < key) = lookup k l
                             | (k > key) = lookup k r
-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Empty = Leaf k v
insert (k, v) (Leaf key value) | (k == key) = Leaf key value
                               | (k > key) = Node key value Empty (Leaf key value) 
                               | (k < key) = Node key value (Leaf key value) Empty
insert (k, v) (Node key value l r) | (k == key) = Node key value l r
                                   | (k < key) = Node key value (insert (k,v) l) r
                                   | (k > key) = Node key value l (insert(k,v) r)
-- Удаление элемента по ключу
putInLeft x Empty = x
putInLeft x (Leaf key value) = Node key value x Empty
putInLeft x (Node key value l r) = Node key value (putInLeft x l) r

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
nearestLE i (Node key value l r)
    | (i == key) = (key,value) 
    | (i < key) = nearestLE i l
    | (i > key) = case r of  Empty -> (key,value) 
                             (Leaf k v) -> nearestLE i (Leaf k v)
                             (Node  k v _ _) | (i == k) -> (k,v)
                             (Node  k v _ _) | (i /= k) -> nearestLE i r
                                    
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
treeSize Empty            = 0
treeSize (Leaf key value) = 1
treeSize (Node _ _ l r) = (treeSize l) + 1 + (treeSize r)


kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i Empty = error "Nothing"
kMean i (Leaf key value) | (i == 0) = (key,value)
                         | otherwise = error "Nothing"
kMean i (Node key value l r) | (treeSize l == i) = (key,value)
                             | (treeSize l < i) = kMean i l
                             | (treeSize l > i) = kMean (i - (treeSize l) - 1) r

--second variant
kMean2 :: Integer -> TreeMap v -> (Integer, v)
kMean2 k Empty = error "EmptyTree"
kMean2 k (Leaf key value) | k == 0 = (key,value)
                          | otherwise = error "Bad index"

kMean2 k f@(Node key value l r) | k < 0 = error "Bad index"
                          | otherwise =  do
                                         let (index, state) = ((execState (kMean' k f) (0, (key,value))))
                                           in if (index <= k) then error "Index too large" else state
  where
    kMean' ::Integer -> TreeMap v -> State (Integer, (Integer, v)) ()
    kMean' _ Empty = return ()
    kMean' k (Leaf key value) = do  
                          (index, state) <- get
                          if (index == k) then put  (index+1, (key,value))  else put (index+1, state)
    kMean' k (Node key value l r) = do
      kMean' k l
      (index, state) <- get
      if (index == k+1)
      then return ()
      else do
           put (index+1, (key, value))
           if (index == k)
           then return ()
           else do
                kMean' k r
                return ()
