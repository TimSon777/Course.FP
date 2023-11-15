module Main (main) where

import MyLib

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Test.Tasty.Hedgehog as Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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

tests = testGroup "All Tests"
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