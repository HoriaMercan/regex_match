module Regex where

data Regex = Anything               -- Anything -> .
           | Choose [Regex]         -- Choose ["a", "bc"] -> (a|bc)
           | Repeat Regex           -- Repeat "a(b|c)" -> (a(b|c))*
           | Plain String           -- Plain "a" -> "a"
           | Compose [Regex]        -- Compose ["a", "bc", "d"] -> "abcd"
    deriving Eq

-- Function for debug purposes
printRegex :: Regex -> String
printRegex = printHelper 0
    where
        printHelper :: Int -> Regex -> String
        printHelper level regex =
            replicate level '\t' ++ case regex of
                Anything ->
                    "- Anything (.)\n"
                Choose regexes ->
                    "- Choose (|)\n" ++
                    concat [printHelper (level + 1) r | r <- regexes]
                Repeat r ->
                    "- Repeat (*)\n" ++
                    printHelper (level + 1) r
                Plain str ->
                    "- Plain \"" ++ str ++ "\"\n"
                Compose regexes ->
                    "- Compose\n" ++
                    concat [printHelper (level + 1) r | r <- regexes]

instance Show Regex where
    show = printRegex

startsWithAnything:: Regex-> Bool
startsWithAnything regex = case regex of
    Anything -> True
    Repeat Anything -> True
    Compose (Anything:_) -> True
    Plain ('.':_) -> True
    Choose l -> all startsWithAnything l
    _ -> False

compose :: [Regex] -> Regex
compose [] = Plain ""
compose [x] = x
compose ((Plain x):(Plain y):l) = compose (Plain (x++y):l)
compose [Plain x, Compose (Plain y:l)] = compose (Plain (x++y):l)
compose [Plain x, Compose l] = compose (Plain x:l)
-- compose (Anything:Anything:l) = compose (Repeat Anything:l)
compose (Anything:Repeat Anything:l) = compose (Repeat Anything:l)
compose (Repeat Anything:Anything:l) = compose (Repeat Anything:l)
compose (Repeat Anything:Repeat Anything:l) = compose (Repeat Anything:l)
compose (x:xs) = Compose (x:xs)

-- Return value will represent the remaining regex and the value of the question
canBeEndingWithAnything::Regex -> (Regex, Bool)
canBeEndingWithAnything r = case r of
    Anything -> (Plain "", True)
    Repeat Anything -> (Plain "", True)
    Compose l -> if last l == Repeat Anything then (compose (take (length l - 1) l), True) else (Anything, False)
    _ -> (Anything, False)

choose :: [Regex] -> Regex
choose [] = Plain ""
choose [x] = x
choose r@[Plain x, Plain y] =
    if (length x) < 2 || (length y) < 2 then Choose r else
    let endingChar = last x
        sameChar = (last x == last y)
        x_ = take (length x - 1) x
        y_ = take (length y - 1) y in
    if sameChar then
        compose [choose [Plain x_, Plain y_], Plain [endingChar]]
    else Choose r

choose l = let intermediary = map canBeEndingWithAnything l in
    if all snd intermediary then
        compose [choose (map fst intermediary), Repeat Anything]
    else
        Choose l

writeRegex :: Regex -> String
writeRegex regex = case regex of
    Anything -> "."
    Plain s  -> s
    Repeat r -> writeRegex r ++ "*"
    Choose [] -> ""
    Compose [] -> ""
    Choose [x] -> writeRegex x
    Compose [x] -> writeRegex x
    Choose (x:xs)  -> "(" ++ foldr (\el acc -> acc ++ "|" ++ writeRegex el) (writeRegex x) xs ++  ")"
    Compose (x:xs) -> foldl (\acc el -> acc ++ writeRegex el) (writeRegex x) xs

writeRegexReversed :: Regex -> String
writeRegexReversed regex = case regex of
    Anything -> "."
    Plain s  -> reverse s
    Repeat r -> writeRegexReversed r ++ "*"
    Choose [] -> ""
    Compose [] -> ""
    Choose [x] -> writeRegexReversed x
    Compose [x] -> writeRegexReversed x
    Choose (x:xs)  -> "(" ++ foldr (\el acc -> acc ++ "|" ++ writeRegexReversed el) (writeRegexReversed x) xs ++  ")"
    Compose l -> foldr (\el acc -> acc ++ writeRegexReversed el) (writeRegexReversed (last l)) (take (length l - 1) l)

-- Returns a list of possible remaining strings after matching
match :: Regex -> String -> [String]
match Anything "" = [""]
match Anything (_:cs) = [cs]
match (Plain "") s = [s]
match (Plain (p:ps)) (s:ss)
    | p == s = match (Plain ps) ss
    | otherwise = []
match (Plain _) "" = []
match (Choose rs) s =                  -- Try all alternatives
    concatMap (`match` s) rs
match (Repeat r) s =                   -- Match zero or more occurrences
    s : do                          -- Zero occurrences
        s' <- match r s                -- Match one occurrence
        match (Repeat r) s'            -- Recursively match more
match (Compose []) s = [s]             -- Empty composition
match (Compose (r:rs)) s = do          -- Match first, then rest
    s' <- match r s
    match (Compose rs) s'

matches :: Regex -> String -> Bool
matches r s = "" `elem` match r s