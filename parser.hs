import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
        deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumberBind

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    x <- oneOf "nrt\\\""
    return $ case x of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _   -> x
 
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChar <|> noneOf "\\\""
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumberDo :: Parser LispVal
parseNumberDo = do
    d <- many1 digit
    return $ Number $ read d

parseNumberBind :: Parser LispVal
parseNumberBind = many1 digit >>= return . Number . read


