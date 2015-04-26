import Data.HashMap.Strict as H
import Data.List
-- Initial types

type ForthState = (IStack, CStack, Dictionary)

type IStack = [Integer]
initialIStack = []

type CStack = [[String]]
initialCStack = []

-- Type for the symbol dictionary



type Dictionary = H.HashMap String [Entry]

operators = [   ("dup",Prim dup),
                ("drop", Prim dropJ),
                ("swap", Prim swap),
                ("rot", Prim rot),
                ("-", Prim (wrap3 (-))),
                ("*", Prim (wrap2 (*))),
                ("+", Prim (wrap2 (+))),
                ("/", Prim (wrap2 div)) ]

initialDictionary [] = H.empty
initialDictionary (x:xs)= dinsert (fst x)(snd x)(initialDictionary xs)

data Entry =
     Prim ([Integer] -> [Integer])
   | Def [String]
   | Num Integer
   | Unknown String

instance Show Entry where
  show (Prim f)    = "Prim"
  show (Def s)     = show s
  show (Num i)     = show i
  show (Unknown s) = "Unknown: " ++ s

-- Dictionary helpers

wrap2 f (x:y:xs) = (f x y):xs
wrap2 f _ = error "Value stack underflow!"

wrap3 f (x:y:xs) = (f y x):xs
wrap3 f _ = error "Value stack underflow!"




-- Helper functions for the language.

-- DUP Function
dup (x:xs) = x:x:xs
dup _ = error "Stack underflow!"

-- drop Function //J was added to avoid conflict with Haskell Library Standard drop dunction.
dropJ (x:xs) = xs
dropJ _ = error "Stack underflow!"

-- Swap Function
swap (x:(y:ys)) = y : x : ys
swap _ = error "Stack underflow!"

rot (x:(y:(z:zs))) = z : x : y : zs
rot _ = error "Stack underflow!"

define word xx (istack,cstack,dict) =
  case elemIndex ";" xx of
    Just index -> define' word (splitAt index xx)(istack,cstack,dict)
    Nothing  -> error "Nothing"
define' _ ([], _) (_, _, _)  = error "No definitions"
define' word (xx,yy) (i,c,d) = eval (tail yy) (i, c, dinsert word (Def xx) d)

-- reverse Stack Print
printStack [] = []
printStack (x:[]) = show x
printStack (x:xs) = printStack xs ++ " " ++ show x



dlookup :: String -> Dictionary -> Entry
dlookup word dict =
  case H.lookup word dict of
    Nothing -> case reads word of
                 [(i,"")] -> Num i
                 _        -> Unknown word
    Just x  -> head x

dinsert :: String -> Entry -> Dictionary -> Dictionary
dinsert key val dict =
   case H.lookup key dict of
      Nothing -> H.insert key [val] dict
      Just x  -> H.insert key (val:x) dict

-- Initial Dictionary

dictionary = initialDictionary(operators)



-- The Evaluator

eval :: [String] -> ForthState -> IO ForthState
eval []    (istack, [],     dict) = return (istack, [], dict)
eval words (istack, cstack, dict) =
  case dlookup (head words) dict of
    Num i        -> eval xs (i:istack, cstack, dict)
    Prim f       -> eval xs (f istack, cstack, dict)
    Unknown "."  -> do { putStrLn $ show (head istack);
                             eval xs (tail istack, cstack, dict) }

    Unknown ".S" -> do { putStrLn $ printStack (istack) ;
                            eval xs (istack, cstack, dict)   }

    Unknown ":"   -> define (head xs) (tail xs) (istack, cstack, dict)


  where xs = tail words

repl :: ForthState -> IO ForthState
repl state =
  do putStr "> " ;
     input <- getLine
     nustate <- eval (words input) state
     repl nustate

main = do
  putStrLn "Welcome to your forth interpreter!"
  repl (initialIStack, initialCStack, dictionary)
