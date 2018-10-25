module Task1_2 where
import Prelude hiding(gcd,sin,cos)
import Todo(todo)
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = sum [sinTerm x i | i <- [0..200]]
      where sinTerm x i = (x^oddTerm / fromIntegral (factorial oddTerm))*(-1)^i
                    where  oddTerm = 2*i + 1

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = sum [cosTerm x i | i <- [0..200]]
      where cosTerm x i = (x^oddTerm / fromIntegral (factorial oddTerm))*(-1)^i
                    where  oddTerm = 2*i
-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd 0 0 = error "all zeros."
gcd x y = gcd' (abs x) (abs y)
    where gcd' x 0 = x
          gcd' x y = gcd' y (x `rem` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo


-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = day>0 && month>0 && month<13 && year>=0 && day <=
    (if month `elem` [4,6,9,11] then 30
     else 
        if month == 2 then
            if (year `mod` 400 == 0) || (year `mod` 100 /= 0) && 
                (year `mod` 4 == 0) then 29
            else 28
        else 31)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = todo

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = null [ y | y <- [2..x - 1], x `mod`y  == 0]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
