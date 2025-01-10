{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Trie
    ( Trie           -- export the type
    , emptyTrie      -- export constructors and functions
    , insert
    , WhichSet
    , wsAccept
    , wsFail
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

data WhichSet = WSAccept | WSFail | WSNone deriving Show


-- | Trie data structure
-- Each node contains:
-- - a character (Maybe Char is Nothing for root)
-- - a boolean indicating if it's end of word
-- - a WhichSet variable concluding which set the child trie 
-- - list of child nodes
data Trie =
    Trie {
        isEnd :: Bool,
        char :: Maybe Char,
        whichSet :: WhichSet,
        children :: Map Char Trie
    }

-- | Function for beauty printing the Trie. Can be used for debugging
printTrie :: Trie -> String
printTrie = printHelper 0 ""
    where
        printHelper :: Int -> String -> Trie -> String
        printHelper level prefix trie =
            replicate level '\t' ++ "-" ++ show (char trie) ++ (if isEnd trie then " ." ++ show (whichSet trie) else "") ++ "\n"
             ++  concat [printHelper (level + 1) (prefix ++ [c]) childTrie |
                (c, childTrie) <- Map.toList (children trie)]
instance Show Trie where
    show = printTrie

wsAccept :: WhichSet
wsAccept = WSAccept

wsFail :: WhichSet
wsFail = WSFail


-- | Create an empty trie
emptyTrie :: Maybe Char -> Trie
emptyTrie c = Trie False c WSNone Map.empty

-- | Insert a new word into the trie
insert :: WhichSet -> String -> Trie -> Trie
insert whichSet_ [] trie = trie {isEnd = True, whichSet = whichSet_}
insert whichSet_ (x:xs) trie = 
    let currKids = children trie
        kidTrie = Map.findWithDefault (emptyTrie (Just x)) x currKids
        newKid = insert whichSet_ xs kidTrie
    in trie {children = Map.insert x newKid currKids}


