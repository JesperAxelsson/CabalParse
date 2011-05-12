module Main where

import Text.ParserCombinators.Parsec hiding (spaces)

tst = "    Default available version: 2.4.0.1"

tstBlock = "\n* QuickCheck\n    Synopsis: Automatic testing of Haskell programs\n    Default available version: 2.4.1.1\n    Installed versions: 2.4.0.1\n    Homepage: http://code.haskell.org/QuickCheck\n    License:  BSD3\n\n* base\n    Synopsis: Basic libraries (backwards-compatibility version)\n    Default available version: 3.0.3.2\n    Installed versions: (4.3.1.0)\n    License:  BSD3\n\n"

data CabalInfo = CabalInfo {  title :: String,
                              defaultVersion :: String,
                              installedVersions :: [String]}
                              deriving (Show)
     
simple :: Parser Char
simple = letter

openClose :: Parser Char
openClose = do char '('
               char ')'

parens :: Parser ()
parens = do char '('
            parens
            char ')'
            parens
            <|> return ()

strings :: Parser String
strings = string "foo"            

testOr :: Parser String
testOr = do try (do string "(a"
                    char ')'
                    return "(a)" ) 
            <|> (string "(b)")


nesting :: Parser Int
nesting = do char '('
             n <- nesting
             char ')'
             m <- nesting
             return $ max (n + 1) m
         <|> return 0

word :: Parser String
word = do skipMany (char ' ')
          many1 (alphaNum <|> space)

sentence :: Parser [String]
sentence = do words <- sepBy word separator
              oneOf ".?!:"
              return words

separator :: Parser ()
separator = skipMany (char ',')

readTitle :: Parser String
readTitle = do skipMany (char '*' <|> space)
               many letter

skipSpace :: Parser ()
skipSpace = skipMany (char ' ')

readVersionString :: Parser String
readVersionString = do skipSpace
                       many1 (digit <|> char '.')

redef2 :: Parser [String]
redef2 = do skipMany (space)
            skipMany1 (string "Installed versions:")
            sepBy readVersionString  (char ',')

redef :: Parser String
redef = do skipMany (space)
           skipMany1 (string "Default available version:")
           readVersionString   

ver = "    Default available version: 1.1.0.1"
ver1 = "    Installed versions: 2.2.3.0"
ver2 = "    Installed versions: 2.2.3.0, 2.4.0.1"
ver3 = "    Installed versions: (4.3.1.0)"

block :: Parser String
block = do skipMany (noneOf "* ")
           readTitle
--           char '*'
           
--           newline
block2 :: Parser [String]
block2 = sepBy (many anyChar) (string "\n\n")

blockDiv :: Parser String
blockDiv = do many anyChar
              string "\n"

run :: Show a => Parser a -> String -> IO()
run p input = 
    case (parse p "" input) of 
        Left err -> do putStr "parse error at " 
                       print err
        Right x -> print x                       

