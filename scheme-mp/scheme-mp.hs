import Text.ParserCombinators.Parsec

data Exp = IntExp Integer
         | SymExp String
         deriving (Show)

data Val = IntVal Integer
         | SymVal String

run x = parseTest x

-- Lexicals

adigit = oneOf ['0'..'9']
digits = many1 adigit

-- Grammaticals

anInt = do d <- digits
           return $ IntExp (read d)

anAtom = anInt

anExp = anAtom

-- Evaluator

eval :: Exp -> [(String,Val)] -> Val
eval (IntExp i) env = IntVal i

-- Printer

instance Show Val where
  show (IntVal i) = show i

repl defs =
  do putStr "> "
     l <- getLine
     case parse anExp "Expression" l of
       Right exp -> putStr (show (eval exp defs))
       Left pe   -> putStr (show pe)
     putStrLn ""
     repl defs
