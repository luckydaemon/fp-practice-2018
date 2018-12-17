module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap x (FunMonad y) = FunMonad(x . y)
    
instance Applicative FunMonad where 
    pure x = FunMonad (\y -> x)
    (FunMonad x) <*> (FunMonad y) = FunMonad (\z -> x z $ y z)
    
instance Monad FunMonad where
    return x = FunMonad (\y -> x)
    (>>=) (FunMonad x) y = FunMonad (\z -> fun (y (x z)) z)
