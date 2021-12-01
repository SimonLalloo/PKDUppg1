-- Av Simon Lalloo och Anthony Melinder



-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS





{-  wordCount document
    Makes a list of the amount of times each word appears in the document.
    RETURNS: A list of tuples containing each word and the amount of times it appears in document.
    EXAMPLES:
        wordCount [["a", "rose", "is", "a", "rose"],["but", "so", "is", "a", "rose"]] 
            == [("a",3),("rose",3),("is",2),("but",1),("so",1)]
        wordCount [[]] == []
-}
wordCount :: Document -> WordTally
wordCount doc =  wordCountAux (concat doc)

{-  wordCountAux sentence
    Auxillary function to wordCount to get the amount of times each word appears in the sentence.
    RETURNS: A list of tuples containing each word and the amount of times it appears in sentence.
    EXAMPLES: 
        wordCountAux ["a", "rose", "is", "a", "rose", "but", "so", "is", "a", "rose"] 
            == [("a",3),("rose",3),("is",2),("but",1),("so",1)]   
        wordCountAux [] == [] 
-}
wordCountAux :: Sentence -> WordTally
-- VARIANT: length list
wordCountAux list
    | null list = []
    | otherwise = 
        let 
            word = head list 
        in 
            getCount word list : wordCountAux [ x | x <- list, x /= word]

{-  getCount word sentence
    Gets the wordTally for a specified word in a sentence.
    RETURNS: A tuple with word and the amount of times it appears in sentence.
    EXAMPLES:
        getCount "rose" ["a", "rose", "is", "a", "rose", "but", "so", "is", "a", "rose"] == ("rose",3)
        getCount "test" ["a", "rose", "is", "a", "rose", "but", "so", "is", "a", "rose"] == ("test",0)
-}
getCount :: String -> Sentence -> (String, Int)
getCount word list = (word, length [ x | x <- list, x == word])





{-  adjacentPairs document
    Creates a list of pairs from adjacent words in a document.
    RETURNS: A list of tuples made up out of the adjacent strings from the sentences in document.
    EXAMPLES:
        adjacentPairs [["time", "for", "a", "break"], ["not", "for", "a", "while"]] 
            == [("time","for"),("for","a"),("a","break"),("not","for"),("for","a"),("a","while")]
        adjacentPairs [[], ["a"]] == []
-}
adjacentPairs :: Document -> Pairs
-- VARIANT: length document
adjacentPairs [] = []
adjacentPairs (x:xs) = zip x (tail x) ++ adjacentPairs xs





{-  initialPairs document
    Creates a list of pairs from the first two words in each sentence in a document.
    PRE: All sentences in document contain at least one word.
    RETURNS: A list of tuples made up out of the first two strings from the sentences in document.
    EXAMPLES:
        initialPairs [["time", "for", "a", "break"], ["not", "yet"]]
            == [("time","for"),("not", "yet")]
        initialPairs [["a"]] == []
-}
initialPairs :: Document -> Pairs
-- VARIANT: length document
initialPairs [] = []
initialPairs (x:xs) = initialPairsAux x ++ initialPairs xs

{-  initialPairsAux sentence
    Makes a tuple from the first two words in a sentence.
    PRE: length sentence > 0
    RETURNS: A tuple in a list made up out of the first two strings in sentence.
    EXAMPLES:
        initialPairsAux ["time", "for", "a", "break"]
            == [("time","for")]
        initialPairsAux ["a"] == []
-}
initialPairsAux :: Sentence -> Pairs
initialPairsAux [x] = []
initialPairsAux (a:b:c) = [(a, b)]





{-  finalPairs document
    Creates a list of pairs from the last two words in each sentence in a document.
    PRE: All sentences in document contain at least one word.
    RETURNS: A list of tuples made up out of the last two strings from the sentences in document.
    EXAMPLES:
        finalPairs [["time", "for", "a", "break"], ["not", "yet"], ["one"]]
            == [("a","break"),("not", "yet")]
-}
finalPairs :: Document -> Pairs
-- VARIANT: length document 
finalPairs [] = []
finalPairs (x:xs) = finalPairsAux x ++ finalPairs xs

{-  finalPairsAux sentence
    Makes a tuple from the last two words in the sentence.
    PRE: length sentence > 0
    RETURNS: A tuple in a list made up out of the last two strings in sentence.
    EXAMPLES:
        finalPairsAux ["time", "for", "a", "break"]
            == [("a","break")]
        finalPairsAux ["a"] == []
-}
finalPairsAux :: Sentence -> Pairs
finalPairsAux [x] = []
finalPairsAux list = [(last (init list), last list)]





{-  pairsCount listOfPairs
    Finds how many times each pair appears in a list, without considering the order of elements.
    RETURNS: A list of tuples containing each pair and the amount of times it appears in listOfPairs.
    EXAMPLES:
        pairsCount [("big","bear"),("bear","big"),("big","dog")]
            == [(("big","bear"),2),(("big","dog"),1)]
        pairsCount [] == []
-}
pairsCount :: Pairs -> PairsTally
-- VARIANT: length list
pairsCount list
    | null list = []
    | otherwise =
        getPairTally pair list ++ pairsCount (makeNewList pair list)
            where 
                pair = head list

{-  getPairTally pair listOfPairs
    Calculates how many times a specified pair appears in a list regardless of order of elements in pair.
    RETURNS: A list containing a tuple containing pair and the amount of tuples in listOfPairs
    containing the same elements as pair.
    EXAMPLES:
        getPairTally ("big", "bear") [("big", "bear"), ("bear", "big"), ("big", "dog")]
            == [(("big","bear"),2)]
        getPairTally ("a", "b") [] == [(("a","b"),0)]
-}
getPairTally :: (String, String) -> Pairs -> PairsTally
getPairTally pair list = [(pair, length [ (a,b) | (a,b) <- list, (a,b) == pair || (b,a) == pair] )]

{-  makeNewList pair listOfPairs
    Removes all instances of a specified pair from a list, regardless of order of elements in the pair.
    RETURNS: A list of all tuples in listOfPairs except except those containing the same elements as pair.
    EXAMPLES:
        makeNewList ("big", "bear") [("big", "bear"), ("bear", "big"), ("big", "dog")] 
            == [("big","dog")]
        makeNewList ("big", "fish") [("big", "bear"), ("bear", "big"), ("big", "dog")] 
            == [("big","bear"),("bear","big"),("big","dog")]      
-}
makeNewList :: (String, String) -> Pairs -> Pairs
makeNewList pair list = [ (a, b) | (a,b) <- list, not ((a,b) == pair || (b,a) == pair) ]





{-  neighbours list inputWord
    Finds the amount of times each word appears together with a specified word.
    RETURNS: A list containing all words that appear next to inputWord and the amount of times they do so.
    EXAMPLES: 
        neighbours [(("bear","big"),2),(("big","dog"),1),(("bear","dog"),3)] "big"
            == [("bear",2),("dog",1)]
        neighbours [(("bear","big"),2),(("big","dog"),1),(("bear","dog"),3)] "fish"
            == []        
-}
neighbours :: PairsTally -> String -> WordTally
neighbours list word = [ (pairWord, num) | ((firstWord, secondWord), num) <- getPairsWithWord list word,
                            let 
                                pairWord = if firstWord == word then secondWord else firstWord ]

{-  getPairsWithWord list word
    Finds all pairTallies containing a specified word.
    RETURNS: A list containing only the tallies including word.
    EXAMPLES: 
        getPairsWithWord [(("bear","big"),2),(("big","dog"),1),(("bear","dog"),3)] "big" 
            == [(("bear","big"),2),(("big","dog"),1)]
        getPairsWithWord [(("bear","big"),2),(("big","dog"),1),(("bear","dog"),3)] "fish"
            == []
-}
getPairsWithWord :: PairsTally -> String -> PairsTally
getPairsWithWord list word = [ ((firstWord, secondWord), num) | ((firstWord, secondWord), num) <- list, firstWord == word || secondWord == word]





{-  mostCommonNeighbour PairsTally word
    Finds the most common neighbour to a specified word.
    RETURNS: Just the neighbour to word with the highest count in PairsTally or Nothing.
    EXAMPLES: 
        mostCommonNeighbour [(("bear","big"),2),(("big","dog"),1),(("bear","dog"),3)] "bear" == Just "dog"
        mostCommonNeighbour [(("bear","big"),2),(("big","dog"),1),(("bear","dog"),3)] "test" == Nothing
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour list word =
    let 
        neighbouringWords = neighbours list word 
    in 
        if not (null neighbouringWords)
            then Just (fst (getMostCommonWord (head neighbouringWords) (tail neighbouringWords)))
            else Nothing 

{-  getMostCommonWord (word, count) list
    Finds the word with the highest tally in a WordTally.
    RETURNS: The tuple with the highest tally in the list (or one of the tied for highest).
    EXAMPLES:
        getMostCommonWord ("bear",2) [("bear",2),("dog",1)] == ("bear",2)
        getMostCommonWord ("bear",2) [("bear",2),("dog",1),("fish",2)] == ("fish",2)
        getMostCommonWord ("fish",2) [] == ("fish",2)
-}
getMostCommonWord :: (String, Int) -> WordTally -> (String, Int)
-- VARIANT: length list
getMostCommonWord (word, count) list
    | null list = (word, count)
    | otherwise =
        if count > snd (head list)
            then getMostCommonWord (word, count) (tail list)
            else getMostCommonWord (head list) (tail list)





-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]])

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])

test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])


-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple"
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky"
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a")

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b")

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun")
                                                                  (mostCommonNeighbour input "the")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\""
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\""
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet")

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]




