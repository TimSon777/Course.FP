module Main (main) where

import MyLib

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Test.Tasty.Hedgehog as Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List

main :: IO ()
main = defaultMain tests

testCase1 = testCase
  "1 list is longer than 2" $
  zipLong [1, 2, 3] "AB" @?= [(1, 'A'), (2, 'B'), (3, 'A')]

testCase2 = testCase
  "2 list is longer than 1" $
  zipLong [1, 2] "ABC" @?= [(1, 'A'), (2, 'B'), (1, 'C')]

testCase3 = testCase
  "Lists of equal length" $
  zipLong [1, 2, 3] "ABC" @?= [(1, 'A'), (2, 'B'), (3, 'C')]

testCase4 = testCase
  "1 list is longer than 2" $
  zipLong [1, 2, 3, 4] "AB" @?= [(1, 'A'), (2, 'B'), (3, 'A'), (4, 'B')]

testCase5 = testCase
  "2 list is longer than 1" $
  zipLong [1, 2] "ABCD" @?= [(1, 'A'), (2, 'B'), (1, 'C'), (2, 'D')]

testCase6 = testCase
  "1 list is longer than 2" $
  zipLong [1, 2, 3, 4, 5, 6, 7] "AB" @?= [(1, 'A'), (2, 'B'), (3, 'A'), (4, 'B'), (5, 'A'), (6, 'B'), (7, 'A')]

testCase7 = testCase
  "2 list is longer than 1" $
  zipLong [1, 2] "ABCDEFG" @?= [(1, 'A'), (2, 'B'), (1, 'C'), (2, 'D'), (1, 'E'), (2, 'F'), (1, 'G')]

testCase8 = testCase
  "1 list empty" $
  zipLong [] "ABC" @?= ([] :: [(Int, Char)])

testCase9 = testCase
  "2 list empty" $
  zipLong [1, 2, 3] "" @?= ([] :: [(Int, Char)])

testCase10 = testCase
  "Both lists empty" $
  zipLong [] "" @?= ([] :: [(Int, Char)])

testZipLong [] _  = []
testZipLong _ []  = []
testZipLong xs ys = if length xs > length ys then zip xs (cycle ys) else zip (cycle xs) ys

property_zipLong = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  ys <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  let result = zipLong xs ys
  let expected = testZipLong xs ys
  result === expected

testsZipLong = testGroup "zipLong"
  [
    testGroup "Reused less then 1 cycle" [
      testCase1,
      testCase2
    ],
    testGroup "Same length"
    [
      testCase3
    ],
    testGroup "Reused 1 cycle" [
      testCase4,
      testCase5
    ],
    testGroup "Reused more then 1 cycle" [
      testCase6,
      testCase7
    ],
    testGroup "Edge cases" [
      testCase8,
      testCase9,
      testCase10
    ],
    testGroup "Property" [
      Hedgehog.testProperty "Hedgehog" property_zipLong
    ]
  ]

isCorrect :: Ord a => Tree a -> Bool
isCorrect Empty = True
isCorrect (Node ml v mr) = maybe True isCorrect ml &&
  maybe True isCorrect mr &&
  all (<=v) (maybe [] traversal ml) &&
  all (>=v) (maybe [] traversal mr)

arbitraryTree :: (Int, Int) -> Size -> Gen (Tree Int)
arbitraryTree (minVal, maxVal) 0 = pure empty
arbitraryTree (minVal, maxVal) size = do
  leftSize <- Size <$> Gen.int (Range.linear 0 $ unSize size - 1)
  let rightSize = size - leftSize - 1
  v <- Gen.int $ Range.linear minVal maxVal
  l <- if leftSize == 0
       then pure Nothing
       else Just <$> arbitraryTree (minVal, v) leftSize
  r <- if rightSize == 0
       then pure Nothing
       else Just <$> arbitraryTree (v, maxVal) rightSize
  
  pure $ Node l v r

treeGen :: Gen (Tree Int)
treeGen = Gen.sized $ arbitraryTree (0, 1000)

property_rotateLeft = property $ do
  originalTree <- forAll treeGen
  Hedgehog.assert $ isCorrect originalTree
  Hedgehog.assert $ (sort $ traversal originalTree) == (sort $ traversal $ rotateLeft originalTree)

property_rotateRight = property $ do
  originalTree <- forAll treeGen
  Hedgehog.assert $ isCorrect originalTree
  Hedgehog.assert $ (sort $ traversal originalTree) == (sort $ traversal $ rotateRight originalTree)

testsRotate = testGroup "Rotate"
  [
    testGroup "Rotate"
      [
        Hedgehog.testProperty "Left" property_rotateLeft,
        Hedgehog.testProperty "Right" property_rotateRight
      ]
  ]

tests = testGroup "All tests"
  [
    testsZipLong,
    testsRotate
  ]