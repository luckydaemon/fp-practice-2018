module Task3_2 where

import Todo (todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons xs x) = (rlistToList xs) ++ [x]

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList x = RCons (listToRList $ init x) (last x)

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) _ RNil = False
    (==) RNil _ = False
    (==) (RCons tl hl) (RCons tr hr) = (tl == tr) && (hl == hr)
    

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show (RCons RNil x) = show x
    show (RCons x y) = show x ++ "," ++ show y

instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil RNil = True
    (<=) RNil _ = True
    (<=) _ RNil = False
    (<=) (RCons tl hl) (RCons tr hr) = (tl <= tr) || (hl <= hr)
    
instance Semigroup (ReverseList a) where
    (<>) x y = mappend x y
    
instance Monoid (ReverseList a) where
    mempty = RNil
    mappend RNil x = x
    mappend x RNil = x
    mappend x (RCons y z) = RCons (mappend x y) z
    
instance Functor ReverseList where
    fmap a RNil = RNil
    fmap a (RCons t h) = RCons (fmap a t) (a h)
