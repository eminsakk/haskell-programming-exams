module PE4 where

data DictTree k v = Node [(k, DictTree k v)] | Leaf v deriving Show


newtype Digit = Digit Char deriving (Show, Eq, Ord) 


type DigitTree = DictTree Digit String
type PhoneNumber = [Digit]


-- Part I:

-- toDigit: Safely convert a character to a digit
toDigit :: Char -> Maybe Digit
lists = "0123456789"
toDigit tmpChar = if elem tmpChar lists then Just (Digit tmpChar) else Nothing


-- toDigits: Safely convert a bunch of characters to a list of digits.
-- Particularly, an empty string fails.
toDigits :: String -> Maybe PhoneNumber
isAllDigit::Int ->String -> [Bool]
isAllDigit length tmpStr
    | length <= 0 = []
    | (elem (head tmpStr) lists) = [True] ++ isAllDigit (length - 1) (tail tmpStr)
    | otherwise = [False] ++ isAllDigit (length - 1) (tail tmpStr)

realMenMaker::String -> PhoneNumber
realMenMaker [] = []
realMenMaker strTempCpy = [Digit (head strTempCpy)] ++ (realMenMaker (tail strTempCpy))

toDigits "" = Nothing
toDigits tmpStr = if elem False (isAllDigit (length tmpStr) tmpStr) then Nothing else Just (realMenMaker tmpStr)


-- Part II:
-- Some phonebook business.

-- numContacts: Count the number of contacts in the phonebook

numContacts :: DigitTree -> Int
isLeaf::DigitTree -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False 

getNode:: DigitTree -> [(Digit, DictTree Digit String)]
getNode (Node x) = x

dfsTraversal:: [(Digit, DictTree Digit String)] -> Int
dfsTraversal digitTree = if ((length digitTree) == 0) then 0 
                         else if (isLeaf (snd (head digitTree))) then 1 + dfsTraversal (tail digitTree) 
                         else dfsTraversal (tail digitTree) + (dfsTraversal (getNode (snd (head digitTree))))

numContacts ddigitTree = dfsTraversal (getNode ddigitTree)

    
-- getContacts: Generate the contacts and their phone numbers in order given a tree. 

getContacts :: DigitTree -> [(PhoneNumber, String)]
getContactsHelper:: PhoneNumber -> [(Digit, DictTree Digit String)] -> [(PhoneNumber,String)]
getLeaf :: DigitTree -> String
getLeaf (Leaf x) = x

getContactsHelper _ [] = []
getContactsHelper number list = if (isLeaf (snd (head list))) then [(number ++ [fst (head list)],getLeaf (snd (head list)))] ++ getContactsHelper number (tail list)
                                else getContactsHelper (number ++ [fst (head list)]) (getNode (snd (head list))) ++ getContactsHelper number (tail list)

getContacts digitTree = getContactsHelper [] (getNode digitTree)


-- autocomplete: Create an autocomplete list of contacts given a prefix

autocomplete :: String -> DigitTree -> [(PhoneNumber, String)]
getNumber (Just x) = x
getNumber _  = []

simplifyList:: PhoneNumber -> PhoneNumber -> PhoneNumber
simplifyList tmpStr tmpNumb 
    | (length tmpStr) == 0 && (length tmpNumb) == 0 = [(Digit '-')]
    | (length tmpStr) == 0 = tmpNumb
    | tmpStr /= [] && tmpNumb /= [] && (head tmpStr) == (head tmpNumb) = simplifyList (tail tmpStr) (tail tmpNumb) 
    | otherwise = []


autocompleteHelper ::  [(PhoneNumber,String)] -> PhoneNumber->[(PhoneNumber,String)]
autocompleteHelper [] _ = []
autocompleteHelper _ [] = []

autocompleteHelper basicList phoneList
    | (simplifyList phoneList (fst (head basicList))) == [(Digit '-')] = [([],snd (head basicList))] ++ (autocompleteHelper (tail basicList) phoneList)
    | (simplifyList phoneList (fst (head basicList))) == [] = autocompleteHelper (tail basicList) phoneList
    | otherwise = [(simplifyList phoneList (fst (head basicList)),snd (head basicList))] ++ (autocompleteHelper (tail basicList) phoneList)

autocomplete "" _ = []
autocomplete strPhone phoneBook = autocompleteHelper (getContacts phoneBook) (getNumber (toDigits strPhone))



-- -- Example Trees to test the code

exampleTree :: DigitTree
exampleTree = Node [
    (Digit '1', Node [
        (Digit '3', Node [
            (Digit '7', Node [
                (Digit '8', Leaf "Jones")])]),
        (Digit '5', Leaf "Steele"),
        (Digit '9', Node [
            (Digit '1', Leaf "Marlow"),
            (Digit '2', Node [
                (Digit '3', Leaf "Stewart")])])]),
    (Digit '3', Leaf "Church"),
    (Digit '7', Node [
        (Digit '2', Leaf "Curry"),
        (Digit '7', Leaf "Hughes")])]

areaCodes :: DigitTree
areaCodes = Node [
    (Digit '3', Node [
        (Digit '1', Node [
            (Digit '2', Leaf "Ankara")]),
        (Digit '2', Node [
            (Digit '2', Leaf "Adana"),
            (Digit '6', Leaf "Hatay"),
            (Digit '8', Leaf "Osmaniye")])]),
    (Digit '4', Node [
        (Digit '6', Node [
            (Digit '6', Leaf "Artvin")])])]

