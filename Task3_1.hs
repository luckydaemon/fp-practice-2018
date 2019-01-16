module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа


 
toInt :: WeirdPeanoNumber -> Int
toInt Zero = 0
toInt (Succ a) = (toInt a) + 1
toInt (Pred a) = (toInt a) - 1

toWPN :: Int -> WeirdPeanoNumber
toWPN x | x > 0 = Succ $ toWPN $ x - 1
        | x < 0 = Pred $ toWPN $ x + 1
        | otherwise = Zero
        
normal :: WeirdPeanoNumber -> WeirdPeanoNumber
normal = toWPN . toInt
        
instance Eq WeirdPeanoNumber where
        (==) Zero Zero = True
        (==) Zero _ = False
        (==) _ Zero = False
        (==) a b = (toInt a) == (toInt b)
        (/=) a b = (toInt a) /= (toInt b)
    
instance Show WeirdPeanoNumber where
    show Zero = "zero"
    show (Succ x) = "succ $" ++show x
    show (Pred x) = "pred $" ++show x
    
instance Enum WeirdPeanoNumber where
    toEnum x = toWPN x
    fromEnum x = toInt x

instance Ord WeirdPeanoNumber where
    l `compare` r = case (normal l, normal r) of
                    (Zero, Zero) ->EQ
                    (Zero, Succ r1) -> LT
                    (Zero, Pred r1) -> GT
                    (Succ l1, Succ r1) -> l1 `compare` r1
                    (Succ l1, _)      -> GT
                    (Pred l1, Pred r1) -> l1 `compare` r1
                    (Pred l1, _)      -> LT

instance Bounded WeirdPeanoNumber where
    minBound = toWPN minBound
    maxBound = toWPN maxBound
    
instance Num WeirdPeanoNumber where
    (+) Zero a = a
    (+) a Zero = a
    (+) (Succ a) b = Succ (a+b)
    (+) (Pred a) b = Pred (a+b)
    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) (Succ a) b = b + (a * b)
    (*) (Pred a) b = if (signum a == signum b) then (b + (a * b))
                    else if (signum a /= signum b) && (signum a < Zero) then negate (b + ((negate a) * b))
                    else negate ((negate b) + a * (negate b))   
    signum Zero = Zero
    signum (Succ _) = Succ Zero
    signum (Pred _) = Pred Zero
    negate Zero = Zero
    negate (Succ a) = Pred (negate a)
    negate (Pred a) = Succ (negate a)
    abs a | a < Zero = negate a
          | otherwise = a
    fromInteger a | a == 0 = Zero
                  | a < 0 = Pred (fromInteger (a + 1))
                  | a > 0 = Succ (fromInteger (a - 1)) 
instance Real WeirdPeanoNumber where 
        toRational = toRational . toInt
    
instance Integral WeirdPeanoNumber where
    toInteger Zero = 0
    toInteger (Succ a) = toInteger a + 1
    toInteger (Pred a) = toInteger a - 1
    quotRem  _ Zero = error "Zero division"
    quotRem Zero _ = (Zero,Zero)
    quotRem a b = (x, a - b * x)
        where y | (b > 0) = a
                | (b < 0) = negate a 
              x = divide_help_func (abs a) (abs b) y
divide_help_func :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
divide_help_func a b y | (a > b) = if (y < 0) then Pred (divide_help_func (a - b) b y) 
                                              else Succ (divide_help_func (a - b) b y)
                       | otherwise = Zero
