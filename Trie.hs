{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Trie
    ( Trie
    , emptyTrie
    , insert
    , WhichSet
    , wsAccept
    , wsFail
    , createRegexFromTrie
    , writeRegex
    , Regex
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Regex
    ( Regex(Plain, Repeat, Anything, Compose), compose, choose, writeRegex )

data WhichSet = WSAccept | WSFail | WSNone deriving (Show, Eq)

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


createRegexFromTrie :: Trie -> Regex
createRegexFromTrie trie__ = fst (create trie__) where
    create::Trie -> (Regex, WhichSet)
    create trie =
        if isEnd trie then  (if whichSet trie == WSFail then Plain "" else Repeat Anything, whichSet trie)
        else
            let kids = Map.toList (children trie)
                kidsRegex = map (\(k, v) -> let (regex, ws) = create v
                                            in (k, ws, regex)) kids
                acceptKids = [kid | kid@(_, ws, _) <- kidsRegex,  ws == wsAccept]
                failKids   = [kid | kid@(_, ws, _) <- kidsRegex,  ws == WSFail]
                noneKids   = [kid | kid@(_, ws, _) <- kidsRegex,  ws == WSNone]

                getNewSimpleRegex::[(Char, WhichSet, Regex)] -> [Regex]
                getNewSimpleRegex = map (\(c, _, r) -> Regex.compose [Plain [c], r])

                compactChilds :: [(Char, WhichSet, Regex)] -> Regex
                compactChilds = Regex.choose . map (\(_, _, r) -> r)

            in
                if null acceptKids && null noneKids then
                    (compactChilds failKids, WSFail)
                else if null failKids && null noneKids then
                    (Regex.Repeat Regex.Anything,
                        WSAccept)
                else
                    (Regex.choose (getNewSimpleRegex acceptKids ++ getNewSimpleRegex noneKids), WSNone)

