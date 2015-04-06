{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Maybe

data Exp = IntExp Integer
         | SymExp String
	       | SExp [Exp]
         deriving (Show)

data Val = IntVal Integer
   | SymVal String
	 | PrimVal ([Val] -> Val)
	 | Closure [String] Exp [(String,Val)]
	 | DefVal String Val
	 | Macro [String] Exp [(String,Val)]
	 | Cons Val Val

run x = parseTest x

adigit = oneOf ['0'..'9']
aletter = oneOf ['a'..'z'] <|> oneOf ['A'..'Z']
digits = many1 adigit
letters = many1 aletter

symchars = oneOf "-*+/:'?><="
whitespace = skipMany1 space

anInt = do d <- digits
           return $ IntExp (read d)

anAtom = anInt

aSymbol = do start <- symchars <|> letter
             other <- many (symchars <|> letter <|> digit)
	     return $ SymExp (start : other)

aList = do char '('
	   skipMany space
           res <- sepBy anExp whitespace
	   char ')'
	   return (SExp res)

quote c qtype = do char c
                   val <- anExp
	           return (SExp [SymExp qtype, val])

anExp = anAtom <|> aList <|> quote ',' "unquote"  <|> quote '`' "quasiquote" <|> quote '\'' "quote" <|> aSymbol

--Util Functions
liftIntOp f a x = IntVal $ foldr f a $ map (\ (IntVal y) -> y) x

myfold f [] = True
myfold f (x:y:ys) = case ys of
                      [] -> f x y
		      _  -> f x y && myfold f (y:ys)

toBool (SymVal "t") = True
toBool (SymVal "nil") = False

liftBool True = (SymVal "t")
liftBool False = (SymVal "nil")

liftCompOp f x = liftBool (myfold f $ map (\ (IntVal y) -> y) x)

liftBoolOp f x = liftBool (foldr1 f $ map toBool x)

liftBoolUnaryOp f (x:[]) = liftBool $ f $ toBool x

liftBasicEqOp ((IntVal x):(IntVal y):[]) = liftBool $ x == y
liftBasicEqOp ((SymVal x):(SymVal y):[]) = liftBool $ x == y
liftBasicEqOp x = SymVal "nil"

liftEqOp ((Cons x1 y1):(Cons x2 y2):[]) | toBool $ liftEqOp (x1:x2:[]) = liftEqOp (y1:y2:[])
                                        | otherwise = SymVal "nil"
liftEqOp x = liftBasicEqOp x

liftList (x:xs) = Cons x (liftList xs)
liftList [] = SymVal "nil"

toList (SymVal "nil") = []
toList (Cons x y) = x : toList y

apply fun args env = case lookup fun env of
       Just (PrimVal fn) -> fn args
       Just (Closure cargs exp cenv) -> eval exp (zip cargs args ++ cenv)
       Nothing -> error ("Function " ++ fun ++ " not defined.")

quotify (IntExp i) = IntVal i
quotify (SymExp s) = SymVal s
quotify (SExp []) = SymVal "nil"
quotify (SExp (x:xs)) = Cons (quotify x) (quotify (SExp xs))

unquote (IntVal i) = IntExp i
unquote (SymVal "nil") = SExp []
unquote (SymVal s) = SymExp s
unquote (Cons a b) = SExp $ map unquote $ toList (Cons a b)

quasiquotify (SExp ((SExp ((SymExp "unquote"):x:[])):xs)) env = Cons (eval x env) (quasiquotify (SExp xs) env)
quasiquotify (SExp ((SymExp "unquote"):x:[])) env = eval x env
quasiquotify (SExp (x:xs)) env = Cons (quasiquotify x env) (quasiquotify (SExp xs) env)
quasiquotify x env = quotify x

evalfn (Closure cargs exp cenv) args env = eval exp (zip cargs (map (\a -> eval a env) args) ++ cenv)



-- Evaluator
runtime = [("+", PrimVal $ liftIntOp (+) 0),
           ("*", PrimVal $ liftIntOp (*) 1),
           ("-", PrimVal $ liftIntOp (-) 0),
	   (">", PrimVal $ liftCompOp (>)),
	   ("<", PrimVal $ liftCompOp (<)),
	   ("and", PrimVal $ liftBoolOp (&&)),
	   ("or", PrimVal $ liftBoolOp (||)),
	   ("not", PrimVal $ liftBoolUnaryOp not),
	   ("eq?", PrimVal $ liftBasicEqOp),
	   ("=", PrimVal $ liftEqOp),
	   ("car", PrimVal $ (\ ((Cons x y):[]) -> x)),
	   ("cdr", PrimVal $ (\ ((Cons x y):[]) -> y)),
	   ("list", PrimVal $ liftList)]


eval (IntExp i) env = IntVal i

eval (SymExp s) env = case lookup s env of
                        Just x -> x
			Nothing -> error ("Symbol " ++ s ++ " has no value.")

eval (SExp []) env = SymVal "nil"

eval (SExp ((SymExp "define"):(SymExp fn):(SExp args):body:[])) env = DefVal fn closure
               where closure = (Closure (map (\(SymExp x) -> x) args) body nenv)
	             nenv = (fn, closure) : env

eval (SExp ((SymExp "defmacro"):(SymExp name):(SExp args):body:[])) env = DefVal name closure
               where closure = (Macro (map (\(SymExp x) -> x) args) body env)

eval (SExp ((SymExp "lambda"):(SExp args):body:[])) env = Closure (map (\(SymExp x) -> x) args) body env

eval (SExp ((SymExp "def"):(SymExp var):body:[])) env = DefVal var (eval body env)

eval (SExp ((SymExp "quote"):body:[])) env = quotify body

eval (SExp ((SymExp "quasiquote"):body:[])) env = quasiquotify body env

eval (SExp ((SymExp "cond"):(SExp body):[])) env = evalcond body
                where evalcond (a:b:rest) = case eval a env of
		    	SymVal "nil" -> evalcond rest
                    	_ -> eval b env
		      evalcond [] = SymVal "nil"



eval (SExp ((SymExp "let"):(SExp body):exp:[])) env = eval exp (letenv ++ env)
                where letenv = map (\[(SymExp s),e] -> (s, eval e env)) defns
		      defns = map (\ (SExp x) -> x) body

eval (SExp ((SymExp "cons"):head:tail:[])) env = Cons (eval head env) (eval tail env)

eval (SExp ((SymExp "apply"):(SymExp fn):lst:[])) env = apply fn (toList (eval lst env)) env

eval (SExp ((SymExp "eval"):quoted:[])) env = eval (unquote $ eval quoted env) env

eval (SExp ((SymExp fn):args)) env = case lookup fn env of
       Just (Macro cargs exp cenv)   -> eval (unquote $ eval exp (zip cargs (toList $ quasiquotify (SExp args) env) ++ env)) env
       Just x -> apply fn (map (\a -> eval a env) args) env
       Nothing -> error ("Function " ++ fn ++ " not defined.")


eval (SExp (s:args)) env = evalfn (eval s env) args env



-- Printer
instance Show Val where
  show (IntVal i) = show i
  show (SymVal s) = s
  show (Cons a b)   = "(" ++ aux a b ++ ")"
        where aux x (Cons y z) = show x ++ " " ++ aux y z
	      aux x (SymVal "nil") = show x
	      aux x y = show x ++ " . " ++ show y
  show (DefVal s _) = s
  show (Closure _ _ _) = "*closure*"
  show (Macro a b _) = "Macro " ++ (show a) ++ " " ++ (show b)

repl defs =
  do putStr "> "
     l <- getLine
     case parse anExp "Expression" l of
       Right exp -> putStr (show x) >> putStrLn "" >> repl (modify x)
                      where x = (eval exp defs)
		            modify (DefVal s v) = (s, v) : defs
			    modify _ = defs
       Left pe   -> putStr (show pe) >> putStrLn "" >> repl defs

main = repl runtime
