module PE1 where

import Text.Printf


-- This function takes a Double and rounds it to 2 decimal places as requested in the PE --
getRounded :: Double -> Double
getRounded x = read s :: Double
               where s = printf "%.2f" x


-------------------------------------------------------------------------------------------
convertTL :: Double -> String -> Double
convertTL money currency 
    | currency == "USD" =  getRounded (money / (8.18))
    | currency == "EUR" =  getRounded (money / (9.62)) 
    | currency == "BTC" =  getRounded (money / (473497.31))
    

-------------------------------------------------------------------------------------------

countOnWatch :: [String] -> String -> Int -> Int
countOnWatch listEmployee name days =
    if listEmployee == [] then 0 
    else if days <= 0 then 0
    else if head listEmployee == name then (1 + countOnWatch (tail listEmployee) name (days - 1))
    else if head listEmployee /= name then  countOnWatch (tail listEmployee) name (days - 1)
    else 0
-------------------------------------------------------------------------------------------

encrypt :: Int -> Int
myModulo :: Int -> Int
myModulo number
    | (mod number 3) == 0 = (number - 1)
    | (mod number 4) == 0 = mod (number * 2) 10
    | (mod number 5) == 0= mod (number + 3) 10
    | otherwise = mod (number + 4) 10
encrypt x = let firstdigit = mod x 10
                seconddigit = mod (div x 10) 10
                thirddigit = mod (div (div x 10) 10) 10
                fourthdigit = mod (div (div (div x 10) 10) 10) 10
            in ((myModulo firstdigit) * 10^0) + ((myModulo seconddigit) * 10^1) + ((myModulo thirddigit) * 10^2) + ((myModulo fourthdigit) * 10^3)

-------------------------------------------------------------------------------------------

compoundInterests :: [(Double, Int)] -> [Double]
compoundHelper :: (Double, Int) -> Double


compoundHelper (money, years) 
     |(money >= 10000) && (years >= 2) = money * (1 + (0.115/12))^(12 * years)
     |(money < 10000) && (years >= 2)  = money * (1 + (0.095/12))^(12 * years)
     |(money >= 10000) && (years < 2)  = money * (1 + (0.105/12))^(12 * years)
     |(money < 10000) && (years < 2)   = money * (1 + (0.090/12))^(12 * years)

compoundInterests personList = [getRounded (compoundHelper person)| person <- personList]
