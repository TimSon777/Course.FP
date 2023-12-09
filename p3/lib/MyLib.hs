module MyLib where

import Control.Concurrent.STM
import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Currency = RUB | USD deriving (Show, Eq)
data Account = Account { currency :: Currency, balance :: Int } deriving (Show, Eq)

type UserName = String
type Bank = TVar (Users, Account)
type Users = Map UserName (TVar Accounts)
type Accounts = IntMap Account
type Amount = Int
type AccountId = Int

printBank :: Bank -> IO ()
printBank bank = do
    (usersMap, bankAccount) <- atomically $ readTVar bank

    putStrLn "--- Bank's Account ---"

    putStrLn $ show bankAccount

    putStrLn "--- User Accounts ---"
    forM_ (Map.toList usersMap) $ \(userName, accountsTVar) -> do
        accounts <- atomically $ readTVar accountsTVar
        putStrLn $ "User " ++ userName ++ ":"
        forM_ (IntMap.toList accounts) $ \(accId, acc) -> do
            putStrLn $ " - Account " ++ show accId ++ ": " ++ show acc
    return ()

registerUser :: UserName -> Bank -> STM ()
registerUser name bank = do
    (users, bankAccount) <- readTVar bank
    case Map.lookup name users of
        Just _ -> return ()
        Nothing -> do
            accountsVar <- newTVar IntMap.empty
            writeTVar bank (Map.insert name accountsVar users, bankAccount)

createAccount :: UserName -> Currency -> Bank -> STM Int
createAccount name currency bankState = do
    (users, _) <- readTVar bankState
    case Map.lookup name users of
        Nothing -> error "User does not exist"
        Just accountsVar -> do
            accounts <- readTVar accountsVar
            let accountId = if IntMap.null accounts then 1 else fst (IntMap.findMax accounts) + 1
            let account = Account { currency = currency, balance = 0 }
            writeTVar accountsVar $ IntMap.insert accountId account accounts
            return accountId

deleteUser :: UserName -> Bank -> STM ()
deleteUser name bankState = do
    (users, bankAccount) <- readTVar bankState
    case Map.lookup name users of
        Just accountsVar -> do
            accounts <- readTVar accountsVar
            when (IntMap.null accounts) $ do
                let updatedUsers = Map.delete name users
                writeTVar bankState (updatedUsers, bankAccount)
        Nothing -> error "User does not exist"

closeAccount :: UserName -> Int -> Bank -> STM ()
closeAccount name accountId bankState = do
    (users, bankAccount) <- readTVar bankState
    case Map.lookup name users of
        Just accountsVar -> do
            accounts <- readTVar accountsVar
            case IntMap.lookup accountId accounts of
                Just account ->
                    if balance account == 0 then
                        writeTVar accountsVar $ IntMap.delete accountId accounts
                    else
                        return ()
                Nothing -> error "Account does not exist"
        Nothing -> error "User does not exist"

putMoney :: UserName -> AccountId -> Currency -> Amount -> Bank -> STM ()
putMoney name accId curr amount bank = do
  (users, bankAcc) <- readTVar bank
  userAccountsVar <- maybe (error "User not found") return (Map.lookup name users)
  userAccounts <- readTVar userAccountsVar
  case IntMap.lookup accId userAccounts of
    Just acc -> do
      let convertedAmount = convertMoney curr (currency acc) amount
      let fee = calculateFee convertedAmount
      let accCurrency = currency acc
      let userUpdatedAcc = acc { balance = balance acc + convertedAmount - fee }
      let updatedAccounts = IntMap.insert accId userUpdatedAcc userAccounts
      writeTVar userAccountsVar updatedAccounts
      let bankFee = convertMoney (currency acc) USD fee
      let bankUpdatedAcc = bankAcc { balance = balance bankAcc + bankFee }
      writeTVar bank (users, bankUpdatedAcc)
    Nothing -> error "Account not found"

withdrowMoney :: UserName -> AccountId -> Amount -> Bank -> STM (Amount, Currency)
withdrowMoney name accId amount bank = do
  (users, bankAcc) <- readTVar bank
  userAccountsVar <- maybe (error "User not found") return (Map.lookup name users)
  userAccounts <- readTVar userAccountsVar
  case IntMap.lookup accId userAccounts of
    Just acc -> do
      if amount > balance acc then do
        let curr = currency acc
        return (0, curr)
      else do
        let curr = currency acc
        let fee = calculateFee amount
        let userUpdatedAcc = acc { balance = balance acc - amount }
        let updatedAccounts = IntMap.insert accId userUpdatedAcc userAccounts
        writeTVar userAccountsVar updatedAccounts
        let bankUpdatedAcc = bankAcc { balance = balance bankAcc + fee }
        writeTVar bank (users, bankUpdatedAcc)
        return $ (amount - fee, curr)
    Nothing -> error "Account not found"

transferMoney :: UserName -> AccountId -> UserName -> AccountId -> Amount -> Bank -> STM Bool
transferMoney fromUser fromAccId toUser toAccId amount bank = do
  (actualAmount, curr) <- withdrowMoney fromUser fromAccId amount bank
  if actualAmount > 0 then do
    putMoney toUser toAccId curr actualAmount bank
    return True
  else
    return False

internalTransferMoney :: UserName -> AccountId -> AccountId -> Amount -> Bank -> STM Bool
internalTransferMoney user fromAccId toAccId amount bank = transferMoney user fromAccId user toAccId amount bank

calculateFee :: Int -> Int
calculateFee amount = max 1 (amount `div` 100)

convertMoney :: Currency -> Currency -> Amount -> Amount
convertMoney USD RUB amount = amount * 100
convertMoney RUB USD amount = amount `div` 100
convertMoney _ _ amount = amount

initBank :: IO (TVar (Map k a, Account))
initBank = newTVarIO (Map.empty, Account { currency = USD, balance = 0 })

main :: IO ()
main = do
    bank <- initBank
    atomically $ registerUser "Alice" bank
    atomically $ createAccount "Alice" USD bank
    atomically $ createAccount "Alice" RUB bank
    atomically $ putMoney "Alice" 1 USD 100 bank
    atomically $ withdrowMoney "Alice" 1 10 bank
    atomically $ transferMoney "Alice" 1 "Alice" 2 44 bank
    printBank bank