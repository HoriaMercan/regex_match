{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import System.IO()
import Trie
import Regex (writeRegexReversed)

-- | Simple data type for storing the languages
data Languages = Languages {
    acceptWords :: [String],
    failWords :: [String],
    wordLength :: Int
} deriving Show

-- | Read languages from file
readLanguagesFromFile :: FilePath -> IO Languages
readLanguagesFromFile filename = do
    contents <- readFile filename
    let allLines = lines contents
    let firstLine = head allLines
    let [cntAccept, cntFail, lenString] = map read $ words firstLine
    let rest = tail allLines
    let acceptWords' = take cntAccept rest
    let failWords' = take cntFail $ drop cntAccept rest
    return Languages {
        acceptWords = acceptWords',
        failWords = failWords',
        wordLength = lenString
    }

-- tryAndOptimise:: Trie -> Regex
-- tryAndOptimise trie_ = 


-- | Main function
main :: IO ()
main = do
    langs <- readLanguagesFromFile "input.txt"
    let _trie =  foldr (insert wsAccept) (emptyTrie Nothing) (acceptWords langs)
        trie = foldr (insert wsFail) _trie (failWords langs)
        _trie_r = foldr (insert wsAccept . reverse) (emptyTrie Nothing) (acceptWords langs)
        trie_r = foldr (insert wsFail . reverse) _trie_r (failWords langs)
    -- putStrLn $ "Word length: " ++ show (wordLength langs)
    -- putStrLn $ "Accept words: " ++ show (acceptWords langs)
    -- putStrLn $ "Fail words: " ++ show (failWords langs)
    let regex = createRegexFromTrie trie 
        regex_r = createRegexFromTrie trie_r
        regex_str = writeRegex regex
        regex_r_str = writeRegexReversed regex_r in do
        writeFile "output.txt" ("regex\n" ++ 
            if length regex_str < length regex_r_str then
                regex_str
            else regex_r_str)

