module Task4_2(main) where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap x (FourOf x1 x2 x3 x4) = FourOf (x x1) (x x2) (x x3) (x x4)

instance Applicative FourOf where
    pure x = FourOf x x x x
    FourOf x1 x2 x3 x4 <*> FourOf y1 y2 y3 y4 = FourOf (x1 y1) (x2 y2) (x3 y3) (x4 y4)

instance Monad FourOf where
    return x = FourOf x x x x
    (>>=) (FourOf x1 x2 x3 x4) y = FourOf (y1 (y x1)) (y2 (y x2))(y3 (y x3))(y4 (y x4)) where
        y1 (FourOf x _ _ _) = x
        y2 (FourOf _ x _ _) = x
        y3 (FourOf _ _ x _) = x
        y4 (FourOf _ _ _ x) = x
        
main = do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y }
