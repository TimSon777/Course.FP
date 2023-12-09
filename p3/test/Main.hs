module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import MyLib
import Data.Maybe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ testGroup "registerUser"
    [ testCase "should register, when doesn't exist" registerUser_1
    , testCase "should not register, when exists"    registerUser_2
    ]
  ,
    testGroup "createAccount"
    [ testCase "should create account" createAccount_1
    ]
  , testGroup "deleteUser"
    [ testCase "should delete user, when no accounts"        deleteUser_1
    , testCase "should not delete user, when account exists" deleteUser_2
    ]
  , testGroup "deleteAccount"
    [ testCase "should delete account, when account is empty"         closeAccount_1
    , testCase "should not delete account, when account is not empty" closeAccount_2
    ]
  , testGroup "putMoney"
    [ testCase "should put money" putMoney_1
    ]
  , testGroup "withdrowMoney"
    [ testCase "should withdrow money, when enough"         withdrowMoney_1
    , testCase "should not withdrow money, when not enough" withdrowMoney_2
    ]
  , testGroup "transferMoney"
    [ testCase "should transfer money, when enough"         transferMoney_1
    , testCase "should not transfer money, when not enough" transferMoney_2
    ]
  , testGroup "internalTransferMoney"
    [ testCase "should transfer money, when enough"         internalTransferMoney_1
    , testCase "should not transfer money, when not enough" internalTransferMoney_2
    ]
  , testGroup "calculateFee"
    [ testCase "should return normal fee" calculateFee_1
    , testCase "should return 1, when minimum" calculateFee_2
    ]
  , testGroup "converMoney"
    [ testCase "should return correct result, when USD -> RUB" convertMoney_1
    , testCase "should return correct result, when RUB -> USD" convertMoney_2
    , testCase "should return correct result, when RUB -> RUB" convertMoney_3
    , testCase "should return correct result, when USD -> USD" convertMoney_4
    ]
  ]

registerUser_1 :: Assertion
registerUser_1 = do
  -- arrange
  bank <- initBank
  let userName = "Timur"

  -- act
  atomically $ registerUser userName bank

  -- assert
  (registeredUsers, _) <- atomically $ readTVar bank
  let userAccountsVar = Map.lookup userName registeredUsers

  assertBool "User should be registered" (isJust userAccountsVar)

  accs <- atomically $ readTVar $ fromJust userAccountsVar
  assertBool "User should have no accounts upon registration" $ IntMap.null accs 

registerUser_2 :: Assertion
registerUser_2 = do
  -- arrange
  bank <- initBank
  let userName = "Timur"

  -- act
  atomically $ registerUser userName bank
  atomically $ createAccount userName USD bank
  atomically $ registerUser userName bank

  -- assert
  (registeredUsers, _) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName registeredUsers

  accs <- atomically $ readTVar userAccountsVar
  assertBool "User should not be recreated" $ not $ IntMap.null accs

createAccount_1 :: Assertion
createAccount_1 = do
  -- arrange
  bank <- initBank
  let userName = "Timur"

  atomically $ registerUser userName bank

  -- act
  atomically $ createAccount userName USD bank

  -- assert
  (registeredUsers, _) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName registeredUsers
  userAccounts <- atomically $ readTVar userAccountsVar
  let Just acc = IntMap.lookup 1 userAccounts

  currency acc @?= USD
  balance acc @?= 0

deleteUser_1 :: Assertion
deleteUser_1 = do
  bank <- initBank
  let userName = "Timur"

  atomically $ registerUser userName bank

  -- act
  atomically $ deleteUser userName bank

  -- assert
  (users, _) <- atomically $ readTVar bank
  let userAccountsVar = Map.lookup userName users

  assertBool "User should be deleted" (isNothing userAccountsVar)

deleteUser_2 :: Assertion
deleteUser_2 = do
  bank <- initBank
  let userName = "Timur"

  atomically $ registerUser userName bank
  atomically $ createAccount userName USD bank

  -- act
  atomically $ deleteUser userName bank

  -- assert
  (users, _) <- atomically $ readTVar bank
  let userAccountsVar = Map.lookup userName users

  assertBool "User should not be deleted" (isJust userAccountsVar)

closeAccount_1 :: Assertion
closeAccount_1 = do
  bank <- initBank
  let userName = "Timur"

  atomically $ registerUser userName bank
  atomically $ createAccount userName USD bank

  -- act
  atomically $ closeAccount userName 1 bank

  -- assert
  (users, _) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName users
  accs <- atomically $ readTVar userAccountsVar
  assertBool "Nothing accounts" $ IntMap.null accs

closeAccount_2 :: Assertion
closeAccount_2 = do
  bank <- initBank
  let userName = "Timur"

  atomically $ registerUser userName bank
  atomically $ createAccount userName USD bank
  atomically $ putMoney userName 1 USD 100 bank

  -- act
  atomically $ closeAccount userName 1 bank

  -- assert
  (users, _) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName users
  accs <- atomically $ readTVar userAccountsVar
  assertBool "Nothing accounts" $ not $ IntMap.null accs

putMoney_1 :: Assertion
putMoney_1 = do
  -- arrange
  bank <- initBank
  let userName = "Timur"
  atomically $ registerUser userName bank
  newAccountId <- atomically $ createAccount userName USD bank

  -- act
  atomically $ putMoney userName newAccountId RUB 10000 bank

  -- assert
  (users, bankAccount) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName users
  userAccounts <- atomically $ readTVar userAccountsVar
  let Just account = IntMap.lookup newAccountId userAccounts

  balance account @?= 99
  currency account @?= USD
  balance bankAccount @?= 1

withdrowMoney_1 :: Assertion
withdrowMoney_1 = do
  -- arrange
  bank <- initBank
  let userName = "Timur"
  atomically $ registerUser userName bank
  newAccountId <- atomically $ createAccount userName USD bank
  atomically $ putMoney userName newAccountId RUB 10000 bank

  -- act
  (amount, _) <- atomically $ withdrowMoney userName newAccountId 10 bank

  -- assert
  (users, bankAccount) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName users
  userAccounts <- atomically $ readTVar userAccountsVar
  let Just account = IntMap.lookup newAccountId userAccounts

  amount @?= 9
  balance account @?= 89
  currency account @?= USD
  balance bankAccount @?= 2
  
withdrowMoney_2 :: Assertion
withdrowMoney_2 = do
  -- arrange
  bank <- initBank
  let userName = "Timur"
  atomically $ registerUser userName bank
  newAccountId <- atomically $ createAccount userName USD bank
  atomically $ putMoney userName newAccountId RUB 10000 bank

  -- act
  (amount, _) <- atomically $ withdrowMoney userName newAccountId 100 bank

  -- assert
  (users, bankAccount) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName users
  userAccounts <- atomically $ readTVar userAccountsVar
  let Just account = IntMap.lookup newAccountId userAccounts

  amount @?= 0
  balance account @?= 99
  currency account @?= USD
  balance bankAccount @?= 1

transferMoney_1 :: Assertion
transferMoney_1 = do
  -- arrange
  bank <- initBank

  atomically $ registerUser "Timur" bank
  timurAccountId <- atomically $ createAccount "Timur" USD bank
  atomically $ putMoney "Timur" timurAccountId RUB 10000 bank

  atomically $ registerUser "Danil" bank
  danilAccountId <- atomically $ createAccount "Danil" RUB bank
  atomically $ putMoney "Danil" danilAccountId RUB 300 bank

  -- act
  isOK <- atomically $ transferMoney "Timur" timurAccountId "Danil" danilAccountId 10 bank

  -- assert
  (users, bankAccount) <- atomically $ readTVar bank

  let Just timurAccountsVar = Map.lookup "Timur" users
  timurAccounts <- atomically $ readTVar timurAccountsVar
  let Just timurAccount = IntMap.lookup timurAccountId timurAccounts

  let Just danilAccountsVar = Map.lookup "Danil" users
  danilAccounts <- atomically $ readTVar danilAccountsVar
  let Just danilAccount = IntMap.lookup danilAccountId danilAccounts

  isOK @?= True
  balance timurAccount @?= 89
  balance danilAccount @?= 1188
  balance bankAccount @?= 2

transferMoney_2 :: Assertion
transferMoney_2 = do
  -- arrange
  bank <- initBank

  atomically $ registerUser "Timur" bank
  timurAccountId <- atomically $ createAccount "Timur" USD bank
  atomically $ putMoney "Timur" timurAccountId RUB 10000 bank

  atomically $ registerUser "Danil" bank
  danilAccountId <- atomically $ createAccount "Danil" RUB bank
  atomically $ putMoney "Danil" danilAccountId RUB 300 bank

  -- act
  isOK <- atomically $ transferMoney "Timur" timurAccountId "Danil" danilAccountId 1000 bank

  -- assert
  (users, bankAccount) <- atomically $ readTVar bank

  let Just timurAccountsVar = Map.lookup "Timur" users
  timurAccounts <- atomically $ readTVar timurAccountsVar
  let Just timurAccount = IntMap.lookup timurAccountId timurAccounts

  let Just danilAccountsVar = Map.lookup "Danil" users
  danilAccounts <- atomically $ readTVar danilAccountsVar
  let Just danilAccount = IntMap.lookup danilAccountId danilAccounts

  isOK @?= False

internalTransferMoney_1 :: Assertion
internalTransferMoney_1 = do
  -- arrange
  bank <- initBank
  let userName = "Timur"
  atomically $ registerUser userName bank
  accId1 <- atomically $ createAccount userName USD bank
  accId2 <- atomically $ createAccount userName RUB bank
  atomically $ putMoney userName accId1 RUB 10000 bank

  -- act
  isOK <- atomically $ internalTransferMoney userName accId1 accId2 10 bank

  -- assert
  (users, bankAccount) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName users
  userAccounts <- atomically $ readTVar userAccountsVar
  let Just account1 = IntMap.lookup accId1 userAccounts
  let Just account2 = IntMap.lookup accId2 userAccounts

  isOK @?= True
  balance account1 @?= 89
  balance account2 @?= 891
  balance bankAccount @?= 2

internalTransferMoney_2 :: Assertion
internalTransferMoney_2 = do
  -- arrange
  bank <- initBank
  let userName = "Timur"
  atomically $ registerUser userName bank
  accId1 <- atomically $ createAccount userName USD bank
  accId2 <- atomically $ createAccount userName RUB bank
  atomically $ putMoney userName accId1 RUB 10000 bank

  -- act
  isOK <- atomically $ internalTransferMoney userName accId1 accId2 10000 bank

  -- assert
  (users, bankAccount) <- atomically $ readTVar bank
  let Just userAccountsVar = Map.lookup userName users
  userAccounts <- atomically $ readTVar userAccountsVar
  let Just account1 = IntMap.lookup accId1 userAccounts
  let Just account2 = IntMap.lookup accId2 userAccounts

  isOK @?= False

calculateFee_1 :: Assertion
calculateFee_1 = calculateFee 1000 @?= 10

calculateFee_2 :: Assertion
calculateFee_2 = calculateFee 10 @?= 1

convertMoney_1 :: Assertion
convertMoney_1 = convertMoney USD RUB 1 @?= 100

convertMoney_2 :: Assertion
convertMoney_2 = convertMoney RUB USD 100 @?= 1

convertMoney_3 :: Assertion
convertMoney_3 = convertMoney RUB RUB 100 @?= 100

convertMoney_4 :: Assertion
convertMoney_4 = convertMoney USD USD 100 @?= 100