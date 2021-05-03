module PE2 where

-- naturals: The infinite list of natural numbers.
naturals :: [Integer]
naturals = [0,1..]

-- interleave: Interleave two lists, cutting off on the shorter list.
interleave :: [a] -> [a] -> [a]
interleave _ [] = []
interleave [] _ = []
interleave (x1:xs1) (x2:xs2) = x1:x2:(interleave xs1 xs2)

-- integers: The infinite list of integers. Ordered as [0, -1, 1, -2, 2, -3, 3, -4, 4...].
integers :: [Integer]
negatives = [-1,-2..]
integers = (interleave naturals negatives) 

--------------------------------

-- splitOn: Split string on first occurence of character.
splitOn :: Char -> String -> (String, String)
findIndex ::Char -> String -> Int
findIndex token (x:xs)
    | token == x = 1
    | otherwise = 1 + (findIndex token xs)
    
splitOn splitter splitting = if (elem splitter splitting) then ((take ((findIndex splitter splitting)-1) splitting), (drop (findIndex splitter splitting) splitting)) 
                             else (splitting,"")

-- tokenizeS: Transform an SJSON string into a list of tokens.
tokenizeS :: String -> [String]
tokenizeS "" = []

tokenizeS (x:xs)
    | (x == '{') = ["{"] ++ (tokenizeS xs)
    | (x == '}') = ["}"] ++ (tokenizeS xs)
    | (x == ':') = [":"] ++ (tokenizeS xs)
    | (x == ',') = [","] ++ (tokenizeS xs)
    | (x == '\'') = let myTuple = splitOn x xs
                    in [fst myTuple] ++ (tokenizeS (snd myTuple))
    |otherwise = tokenizeS xs



-- prettifyS: Prettify SJSON.
prettifyS :: String -> String
helperPrettifyS :: [String]-> Int -> String
myRepeat:: String -> Int -> String

myRepeat str num = concat $ replicate num str
helperPrettifyS [] _ = []


helperPrettifyS (x:lst) bracketTracker 
    | x == "{" = "{" ++ "\n"  ++  (myRepeat " " (4*(bracketTracker + 1))) ++ (helperPrettifyS lst (bracketTracker + 1))
    | x == "}" = "\n" ++ (myRepeat " " (4*(bracketTracker - 1))) ++ "}" ++ (helperPrettifyS lst (bracketTracker - 1))
    | x == ":" = ": " ++ (helperPrettifyS lst (bracketTracker))
    | x == "," = "," ++ "\n" ++ (myRepeat " " (4*(bracketTracker))) ++ (helperPrettifyS lst (bracketTracker))
    | otherwise = "'" ++ x ++ "'" ++ (helperPrettifyS lst bracketTracker)

prettifyS myStr = helperPrettifyS (tokenizeS myStr) 0

