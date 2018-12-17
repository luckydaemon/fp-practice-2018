module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так



-- fmap :: (a -> b) -> f a -> f b => функтор является ковариантным
--но,PSet{ contains :: (a -> Bool) => PSet - контрвариативный
-- => нужно изменить либо PSet либо fmap так, чтобы оба были либо ковариантными либо контрвариативными
class CFunct f where
 cmap :: (a -> b) -> f b -> f a

instance CFunct PSet where
 cmap f (PSet a) = PSet (a . f)
 
 --cmap - контравариантый функтор
 
 
-- Моноид – это тип, отвечающий двум требованиям:
-- Есть нейтральное значение, называемое нулём моноида
-- Есть операция добавления моноида к моноиду
-- Исходя из этого, можно описать объединение, пересечение и симметрическую разность множеств

--Объединение 

newtype Union a = Union{ contains_union :: (a -> Bool) }

instance Monoid (Union a) where
    mempty = Union (\x-> False)
    mappend (Union x) (Union y)  = Union (\z -> x z || y z) 
    
instance Semigroup (Union a) where
    (<>) x y = mappend x y

--Пересечние 

newtype Inter a = Inter{ contains_inter :: (a -> Bool) }

instance Monoid (Inter a) where
    mempty = Inter (\x-> True)
    mappend (Inter x) (Inter y)  = Inter (\z -> x z && y z) 
    
instance Semigroup (Inter a) where
    (<>) x y = mappend x y
    
--Симметрическая разность

newtype SymmDiff a = SymmDiff{ contains_symmdiff :: (a -> Bool) }

instance Monoid (SymmDiff a) where
    mempty = SymmDiff (\x-> False)
    mappend (SymmDiff x) (SymmDiff y)  = SymmDiff (\z -> ((x z) && (not $ y z)) || ((y z) && (not $ x z))) 
    
instance Semigroup (SymmDiff a) where
    (<>) x y = mappend x y
