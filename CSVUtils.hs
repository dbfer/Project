-----------------------------------------------------------------------------
-- |
-- Module      :  CSVUtils
-- Copyright   :  (c) Domagoj Begusic
--
-----------------------------------------------------------------------------

module CSVUtils (User(..),
                 UserIdentifier,
                 Role(..),
                 splitString,
                 userToString,
                 stringToUser,
                 parseCSV,
                 readCSV,
                 showCSV,
                 writeCSV
                 ) where

import System.Environment

-- * Types and utilities

type Separator = String
type Document  = String
type CSV       = [User]

-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String

-- | The user’s role in the course
data Role = Student Integer | TA Integer Integer | Professor deriving (Eq, Ord, Show, Read)

-- | A user
data User = User {
 identifier :: UserIdentifier,
 email :: String,
 pwdHash :: String,
 role :: Role
  } deriving (Eq, Show)

-- | Splits string at given separator.
splitString :: String -> String -> [String]
splitString sep = words . f "" sep
 where f :: String -> String -> String -> String
       f i sep ""     = i
       f i sep (x:xs)
        | [x]==sep    = f (i++" ") sep xs
        | otherwise   = f (i++[x]) sep xs

-- * Implementation

-- | Converts User data to string.
userToString :: User -> String
userToString user@(User a b c d) = a++sep++b++sep++c++sep++show d
 where sep = ";"

-- | Converts string to User data.
stringToUser :: String -> User
stringToUser xs = User a b c (read $ unwords d)
 where (a:b:c:d) = splitString ";" xs

-- | Parses CSV from Document.
parseCSV :: Separator -> Document -> CSV
parseCSV sep doc
 | not $ elem (head sep) doc = error ("The separator '"++sep++"' does not occur in the text")
 | otherwise = [stringToUser userstring | userstring <- tail $ lines doc]

-- | Shows CSV in form of a Document.
showCSV :: Separator -> CSV -> Document
showCSV sep csv = init $ unlines [ userToString user | user <- csv]

-- | Reads CSV from a file.
readCSV :: Separator -> FilePath -> IO CSV
readCSV sep path = do
 doc <- readFile path
 return $ parseCSV sep doc

-- | Writes CSV to a file.
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV sep path csv = do
 writeFile path $ "IDENTIFIER;EMAIL;PASSWORD;ROLE\n" ++ (showCSV sep csv)