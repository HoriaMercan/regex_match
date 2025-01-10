module Main where

import System.IO()
import Trie

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

-- | Main function
main :: IO ()
main = do
    langs <- readLanguagesFromFile "input.txt"
    let _trie =  foldr (insert wsAccept) (emptyTrie Nothing) (acceptWords langs)
        trie = foldr (insert wsFail) _trie (failWords langs)
    putStrLn $ "Word length: " ++ show (wordLength langs)
    putStrLn $ "Accept words: " ++ show (acceptWords langs)
    putStrLn $ "Fail words: " ++ show (failWords langs)
    print trie
