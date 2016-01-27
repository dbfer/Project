-----------------------------------------------------------------------------
-- |
-- Module      :  TemplateParser
-- Copyright   :  (c) Domagoj Begusic
--
-----------------------------------------------------------------------------

module TemplateParser (compileSubs,
                       compileIfs
                      ) where

-- * Types and utilities

import Data.Map (Map)
import qualified Data.Map as Map

-- | Boolean expression.
data Expr a = Val Bool | And (Expr a) (Expr a) | Or (Expr a) (Expr a) | Not (Expr a) deriving (Show)

-- | Evaluates bool expression (Expr).
eval :: Expr a -> Bool
eval (Val True) = True
eval (Val False) = False
eval (And ex1 ex2) = (eval ex1) && (eval ex2)
eval (Or ex1 ex2) = (eval ex1) || (eval ex2)
eval (Not ex) = not (eval ex)

-- | Finds value for given key in Map String String. If there is no such key, returns given word instead.
ifKeyfindValue :: Map String String -> String -> String
ifKeyfindValue m x = case v of
  Just y  -> y
  Nothing -> x
 where v = Map.lookup x m

-- | Checks if given line is beginning of an "if statement".
isIF :: String -> Bool
isIF s = "@if" == (take 3 s)

-- | Checks if given line is "else" part of an "if statement"
isELSE :: String -> Bool
isELSE s = "@else" == (take 5 s)

-- | Checks if given line is ending of an "if statement".
isENDIF :: String -> Bool
isENDIF s = "@endif" == (take 6 s)

-- | Brings out message that needs to be sent if "if statement" condition is true.
valueIF :: String -> String
valueIF = unlines . takeWhile (not . isELSE) . tail . dropWhile (not . isIF) . lines

-- | Brings out message that needs to be sent if "if statement" condition is false.
valueELSE :: String -> String
valueELSE = unlines . takeWhile (not . isENDIF) . tail . dropWhile (not . isELSE) . lines

-- | Brings out unparsed condition of an "if statement".
getCond :: String -> String
getCond = map (\x-> if x=='_' then ' ' else x) . drop 3 . head . filter (isIF) . lines

-- | Treats part of boolean expression wrapped in parentheses as a word.
takeWhile' :: String -> Int -> String -> String
takeWhile' "" n i = if n==0 then i else error "Invalid condition of an if statement!"
takeWhile' (x:xs) n i
 | x=='(' = takeWhile' xs (n+1) (i++[x])
 | x==')' = takeWhile' xs (n-1) (i++[x])
 | n==0   = i
 | n<0    = error "Invalid condition of an if statement!"
 | otherwise = takeWhile' xs n (i++[x])

-- | Acts like function <words> from Prelude but treats part of boolean expression wrapped in parentheses as a word.
myWords :: String -> [String]
myWords "" = []
myWords (x:xs)
 | x==' ' || x==')' = myWords xs
 | x=='('    = (takeWhile' (x:xs) 0 "") : (myWords $ rest (takeWhile' (x:xs) 0 "") (x:xs))
 | otherwise = (takeWhile (/=' ') (x:xs)) : (myWords $ rest (takeWhile (/=' ') (x:xs)) (x:xs))
 where rest :: String -> String -> String
       rest [] ys = ys
       rest (x:xs) (y:ys) = if x==y then rest xs ys else (y:ys)

-- Converts "if statement" condition to bool expresion (Expr). If a variable is undefined, returns an error.
convToExpr :: Map String String -> [String] -> Expr a
convToExpr mp ["True"] = Val True
convToExpr mp ["False"] = Val False
convToExpr mp [x]
 | flag=="True" = convToExpr mp ["True"]
 | flag=="False" = convToExpr mp ["False"]
 where flag = ifKeyfindValue mp x
convToExpr mp (x:xs)
 | x=="Or"   = Or  (convToExpr mp [(head xs)]) (convToExpr mp (tail xs))
 | x=="And"  = And (convToExpr mp [(head xs)]) (convToExpr mp (tail xs))
 | x=="Not"  = Not (convToExpr mp (tail xs))
 | head x=='(' && last x==')' = convToExpr mp $ myWords $ tail $ init $ x
 | otherwise = error "Undefined variable in if statement!"
convToExpr mp _ = error "Invalid variable in if statement!"

-- | Parses "if statement" condition; converts to boolean expression and evaluates it.
parseIF :: Map String String -> String -> Bool
parseIF mp = eval . convToExpr mp . myWords

-- | Compiles "if statement".
compileIF :: Map String String -> String -> String
compileIF mp str = (before ++ r ++ after)
 where i = valueIF $ str
       e = valueELSE $ str
       c = getCond $ str
       r = if parseIF mp c then i else e
       before = unlines $ takeWhile (not . isIF) $ lines $  str
       after = unlines $ tail $ dropWhile (not . isENDIF) $ lines $ str

-- * Implementation

-- | Subtitutes all key words with values from Map String String found in a given string.
compileSubs :: String -> Map String String -> String
compileSubs str m = unlines $ map (unwords) $ map (map (ifKeyfindValue m)) $ map (words) $ lines $ str

-- | Compiles all "if statements" found in a given string.
compileIfs :: Map String String -> String -> String
compileIfs mp str = if c then compileIfs mp (compileIF mp str) else str
 where c = any (isIF) $ lines str