module GlobRegexMod
  (
    globToRegex
  , matchGlobSensitive
  , matchGlobIgnoreCase
  ) where

import Text.Regex.Posix ((=~))
import Data.Char

globToRegex :: String -> Integer -> String
globToRegex cs x = '^' : globToRegex' cs x ++ "$"

globToRegex' :: String -> Integer -> String
globToRegex' "" _ = ""
globToRegex' ('*':cs) x = ".*" ++ globToRegex' cs x
globToRegex' ('?':cs) x = '.' : globToRegex' cs x
globToRegex' ('[':'!':c:cs) x = "[^" ++ (escape c x) ++ charClass cs x
globToRegex' ('[':c:cs) x = '[' : escape c x ++ charClass cs x
globToRegex' ('[':_) _ = error "whatever dude"
globToRegex' (c:cs) x = (escape c x) ++ globToRegex' cs x

escape :: Char -> Integer -> String
escape c x | c `elem` regexChars = '\\' : [c]
           | x == 0 = [c]
           | x == 1 && toLower c == toUpper c = [c]
           | x == 1 = '[' : [toLower c, toUpper c]  ++  "]"
  where regexChars = "\\+()^$.{}|"

charClass :: String -> Integer -> String
charClass (']':cs) x = ']' : globToRegex' cs x
charClass (c:cs) x = escape c x ++  charClass cs x
charClass [] _ = error "this time it's charclass"


matchGlobSensitive :: FilePath -> String -> Bool
name `matchGlobSensitive` pat = name =~ globToRegex pat 0

matchGlobIgnoreCase :: FilePath -> String -> Bool
name `matchGlobIgnoreCase` pat = name =~ globToRegex pat 1
