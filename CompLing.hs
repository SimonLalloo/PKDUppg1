-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)
import Text.XHtml.Strict (docType)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS

{-  wordCount document
    Makes a list of the amount of times each word appears in the
    document
    RETURNS: A list containing tuples with each word and the amount of
    times it appears in document
    EXAMPLES:
-}
wordCount :: Document -> WordTally
wordCount doc = let list = makeOneSentence doc in wordCount' list

{-  wordCount' sentence
    Auxillary function to wordCount to get the amount of times each word appears in the sentence
    RETURNS: A list containing tuples with each word and the amount of
    times it appears in sentence
    EXAMPLES:
-}
wordCount' :: Sentence -> WordTally
wordCount' list -- VARIANT: length list
    | null list = []
    | otherwise = let word = head list in getCount word list : wordCount' [ x | x <- list, x /= word]

{-  makeOneSentence document
    Turns a document into a single sentence
    RETURNS: A sentence containing all strings in the document
    EXAMPLES:
-}
makeOneSentence :: Document -> Sentence
makeOneSentence doc = [y | x <- doc, y <- x]

{-  getCount word sentence
    gets the wordTally for a specified word
    RETURNS: A tuple with the specified word and the amount of times
    it appears in the sentence
    EXAMPLES:
-}
getCount :: String -> Sentence -> (String, Int)
getCount word list = (word, length [ x | x <- list, x == word])






adjacentPairs :: Document -> Pairs
adjacentPairs = undefined  -- remove "undefined" and write your function here

initialPairs :: Document -> Pairs
initialPairs = undefined  -- remove "undefined" and write your function here

finalPairs :: Document -> Pairs
finalPairs = undefined  -- remove "undefined" and write your function here

pairsCount :: Pairs -> PairsTally
pairsCount = undefined  -- remove "undefined" and write your function here


neighbours :: PairsTally -> String -> WordTally
neighbours = undefined  -- remove "undefined" and write your function here

mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here



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




