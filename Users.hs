-----------------------------------------------------------------------------
-- |
-- Module      :  Users
-- Copyright   :  (c) Domagoj Begusic
--
-----------------------------------------------------------------------------

module Users (createUser,
              updateUser,
              deleteUser,
              listUsers,
              listUsersInRole,
              getUser,
              isRoleInYear
              ) where

-- * Types and utilities

import CSVUsers
import Data.Char (chr,ord)
import System.Directory

-- | File path to user database.
dbPath :: String
dbPath = "db.csv"

-- | Checks if e-mail is valid.
checkIfEmail :: String -> Bool
checkIfEmail []                          = error "E-mail address can not be empty."
checkIfEmail xs
 | head xs == '@'                        = error "E-mail address must contain account name."
 | head l1 == xs                         = error "E-mail address must contain character '@'."
 | length l1 == 1                        = error "E-mail address must contain domain."
 | length l2 == 1                        = error "Domain error"
 | otherwise                             = True
 where l1 = splitString "@" xs
       l2 = splitString "." $ last l1

-- | Checks if password is valid.
checkIfPassword :: String -> Bool
checkIfPassword []                            = error "Password can not be empty."
checkIfPassword xs
 | length xs < 6                              = error "Password must contain at least 6 characters."
 | not $ any (\x -> True) $ filter (uppC) xs  = error "Password must contain at least one uppercase letter."
 | not $ any (\x -> True) $ filter (spCh) xs  = error "Password must contain at least one special character."
 | not $ any (\x -> True) $ filter (alpNu) xs = error "Password must contain at least one alphanumeric character."
 | not $ any (\x -> True) $ filter (lowC) xs  = error "Password must contain at least one lowercase letter."
 | otherwise = True
 where uppC x = (ord x)>=65 && (ord x)<=90
       spCh x = ((ord x)>=32 && (ord x)<=47) || ((ord x)>=58 && (ord x)<=64) || ((ord x)>=91 && (ord x)<=96) || ((ord x)>=123 && (ord x)<=126)
       alpNu  x = (ord x)>=48 && (ord x)<=57
       lowC x = (ord x)>=97 && (ord x)<=122

-- | Saves user in database. Checks if user already exists.
saveUser :: User -> IO User
saveUser x@(User a b c d) = do
 tmp <- listUsers
 if []==filter (\(User n _ _ _) -> a==n) tmp
  then if tmp==[]
        then writeFile dbPath (a++";"++b++";"++c++";"++show d)
        else appendFile dbPath ("\n"++a++";"++b++";"++c++";"++show d)
  else error "The user identifier is already taken."
 return x

-- | Hashes password.
hashIt :: String -> String
hashIt xs = loop xs 0 []
 where loop :: String -> Int -> String -> String
       loop [] i r     = r
       loop (n:ns) i r = loop ns (i+1) ([chr $ (ord n)*31^(-1-i+length xs) `mod` 128] ++ r)

-- | Finds position of the user in a given list of users.
findPositionOfUser :: [User] -> User -> Int
findPositionOfUser [] _ = error "Given user is not present."
findPositionOfUser (u:us) v
 | u==v                 = 0
 | otherwise            = 1 + findPositionOfUser us v

-- | Removes user at given position from a given list of users.
removeUserAtPosition :: [User] -> Int -> [User]
removeUserAtPosition us n = map (fst) $ filter (\(a,b) -> b/=n) $ zip us [0..]

-- | Replaces old user with new user at the given position from a given list of users.
changeUserAtPosition :: [User] -> User -> Int -> [User]
changeUserAtPosition us v n = map (\(a,b) -> if b==n then v else a) $ zip us [0..]

-- | Writes list of users in a new database, removes old database, renames new one. All in sequential order.
switchDBFiles :: [User] -> IO ()
switchDBFiles users = do
 writeCSV ";" "tmpDB.csv" users
 removeFile dbPath
 renameFile "tmpDB.csv" dbPath

-- * Implementation

-- | Takes a user identifier, e-mail, password and role.
-- | Performs password hashing and stores the user into the
-- | database, returning a filled User. If creating it fails (e.g.
-- | the user identifier is already taken), throws an appropriate exception.
createUser :: UserIdentifier -> String -> String -> Role -> IO User
createUser id em pwd rol
 | checkIfEmail em && checkIfPassword pwd = saveUser $ User id em (hashIt pwd) rol
 | otherwise = error "E-mail is invalid."

-- | Updates a given user. Identifies it by the UserIdentifier in the User and overwrites
-- | the DB entry with the values in the User structure.
-- | New password MUST be hashed already.
updateUser :: User -> IO ()
updateUser userNEW = do
 users <- listUsers
 userOLD <- getUser (identifier userNEW)
 let f = findPositionOfUser users userOLD
 let c = changeUserAtPosition users userNEW f
 if length c>0
  then
   switchDBFiles c
  else
   return ()

-- | Deletes a user referenced by identifier.
deleteUser :: UserIdentifier -> IO ()
deleteUser id = do
 users <- listUsers
 user <-getUser id
 let f = findPositionOfUser users user
 let r = removeUserAtPosition users f
 if f>=0 then switchDBFiles r else return ()

-- | Lists all the users
listUsers :: IO [User]
listUsers = readCSV ";" dbPath

-- | Lists all users in a given role
listUsersInRole :: Role -> IO [User]
listUsersInRole rol = do
 list <- listUsers
 return $ filter (\(User _ _ _ r) -> r==rol) list

-- | Fetches a single user by identifier
getUser :: UserIdentifier -> IO User
getUser id = do
 list <- listUsers
 let tmpU = filter (\(User a _ _ _) -> a==id) list
 if tmpU == []
  then error "There is no user with that identifier."
  else return $ head tmpU

-- | Checks whether the user has a role of AT LEAST X in a given academic year.
isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear _ (Professor) i            = error "Can not compare user's role with Professor role; Professors do not have academic year specified."
isRoleInYear (User _ _ _ Professor) _ i = error "Professors do not have academic year specified."
isRoleInYear (User _ _ _ r1) r2 i       = atLeastY r1 r2 i
 where atLeastY :: Role -> Role -> Integer -> Bool
       atLeastY (Student x1) (Student _) i = i <= x1
       atLeastY (TA x1 y1)   (TA _ _)    i = i <= (y1-x1)
       atLeastY _            _           _ = False