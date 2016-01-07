-----------------------------------------------------------------------------
-- |
-- Module      :  CSVUsers
-- Copyright   :  (c) Domagoj Begusic
--
-----------------------------------------------------------------------------

module CSVUsers (User(..),
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

-- | Splits string at a given separator.
splitString :: String -> String -> [String]
splitString sep = words . map (\x -> if [x]==sep then ' ' else x)

-- * Implementation

-- | Converts User data to string.
userToString :: User -> Separator -> String
userToString (User a b c d) sep = a++sep++b++sep++c++sep++show d

-- | Converts string to User data.
stringToUser :: String -> Separator -> User
stringToUser xs sep = User a b c (read $ unwords d)
 where (a:b:c:d)    = splitString sep xs

-- | Parses CSV from Document.
parseCSV :: Separator -> Document -> CSV
parseCSV sep doc
 | not $ elem (head sep) doc = error ("The separator '"++sep++"' does not occur in the text")
 | otherwise = map (\x -> stringToUser x sep) $ lines doc

-- | Shows CSV in form of a Document.
showCSV :: Separator -> CSV -> Document
showCSV sep [] = ""
showCSV sep xs = init $ unlines $ map (\x -> userToString x sep) xs

-- | Reads CSV from a file.
readCSV :: Separator -> FilePath -> IO CSV
readCSV sep path = do
 doc <- readFile path
 if doc==""
  then return []
  else return $ parseCSV sep doc

-- | Writes CSV to a file.
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV sep path csv = do
 writeFile path (showCSV sep csv)