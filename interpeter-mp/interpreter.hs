{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec

-- Language Representation

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | LoadStmt String
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
   deriving Show

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
   deriving Show

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | PrimVal String

instance Show Val where
   show (IntVal i) = show i
   show (BoolVal i) = show i
   show (PrimVal i) = show i


-- Parser, given for you this time.

-- Lexicals

symbol s = do string s
              spaces
              return s

int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

var = do v <- many1 letter <?> "an indentifier"
         spaces
         return v

parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

-- Expressions

intExp = do i <- int
            return $ IntExp i

boolExp = do { symbol "true" ; return $ BoolExp True }
      <|> do { symbol "false"; return $ BoolExp False}

varExp = do v <- var
            return $ VarExp v

mulOp =    do { symbol "*" ; return $ IntOpExp "*" }
       <|> do { symbol "/" ; return $ IntOpExp "/" }

addOp =    do { symbol "+" ; return $ IntOpExp "+" }
       <|> do { symbol "-" ; return $ IntOpExp "-" }

andOp = do try $ symbol "and"
           return $ BoolOpExp "and"

orOp = do try $ symbol "or"
          return $ BoolOpExp "or"

compOp =   do { symbol "<" ; return $ CompOpExp "<" }
       <|> do { symbol ">" ; return $ CompOpExp ">" }
       <|> do { symbol "<=" ; return $ CompOpExp "<=" }
       <|> do { symbol ">=" ; return $ CompOpExp ">=" }
       <|> do { symbol "/=" ; return $ CompOpExp "/=" }
       <|> do { symbol "==" ; return $ CompOpExp "==" }

ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           symbol "fi"
           return $ IfExp e1 e2 e3

funExp = do try $ symbol "fn"
            symbol "["
            params <- var `sepBy` (symbol ",")
            symbol "]"
            body <- expr
            symbol "end"
            return $ FunExp params body

letExp = do try $ symbol "let"
            symbol "["
            params <- many $ do v <- var
                                e <- expr
                                return (v,e)
            symbol "]"
            body <- expr
            symbol "end"
            return $ LetExp params body

appExp = do try $ symbol "call"
            efn <- expr
            symbol "("
            exps <- expr `sepBy` (symbol ",")
            symbol ")"
            return $ AppExp efn exps

expr = disj `chainl1` orOp
disj = conj `chainl1` andOp
conj = arith `chainl1` compOp
arith = term `chainl1` addOp
term = factor `chainl1` mulOp
factor = atom

atom = intExp
   <|> ifExp
   <|> try boolExp
   <|> funExp
   <|> appExp
   <|> letExp
   <|> varExp
   <|> parens expr

-- Statements

quitStmt = do try $ symbol "quit"
              symbol ";"
              return QuitStmt

printStmt = do try $ symbol "print"
               e <- expr
               symbol ";"
               return $ PrintStmt e

loadStmt = do try $ symbol "load"
              string "\""
              name <- many1 $ oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".")
              symbol "\""
              symbol ";"
              return $ LoadStmt name

setStmt = do v <- var
             symbol ":="
             e <- expr
             symbol ";"
             return $ SetStmt v e

ifStmt = do try $ symbol "if"
            e1 <- expr
            symbol "then"
            s2 <- stmt
            symbol "else"
            s3 <- stmt
            symbol "fi"
            return $ IfStmt e1 s2 s3

procStmt = do try $ symbol "procedure"
              name <- var
              symbol "("
              params <- var `sepBy` (symbol ",")
              symbol ")"
              body <- stmt
              symbol "endproc"
              return $ ProcedureStmt name params body

callStmt = do try $ symbol "call"
              name <- var
              symbol "("
              args <- expr `sepBy` (symbol ",")
              symbol ")"
              symbol ";"
              return $ CallStmt name args

seqStmt = do try $ symbol "do"
             stmts <- many1 stmt
             symbol "od"
             symbol ";"
             return $ SeqStmt stmts

stmt = quitStmt
   <|> printStmt
   <|> loadStmt
   <|> ifStmt
   <|> procStmt
   <|> callStmt
   <|> seqStmt
   <|> setStmt

-- Type for the symbol dictionary

type Env = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = IO (PEnv,Env)

-- Primitives

intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*)) ]
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||)) ]
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==)) ]

liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y

liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y

liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y

-- The Evaluator

eval :: Exp -> Env -> Val
eval (IntExp i) env = IntVal i
eval (VarExp s) env =
   case H.lookup s env of
     Just v -> v
     Nothing -> IntVal 0

eval (IfExp e1 e2 e3) env =
    let v1 = eval e1 env
     in case v1 of
         BoolVal False -> eval e3 env
         _             -> eval e2 env


eval (CompOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
        Just bop = H.lookup op compOps
    in liftCompOp bop v1 v2


eval (IntOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
        Just bop = H.lookup op intOps
    in liftIntOp bop v1 v2



eval (BoolOpExp op e1 e2) env =
   let v1 = eval e1 env
       v2 = eval e2 env
       Just bop = H.lookup op boolOps
    in liftBoolOp bop v1 v2


eval (FunExp params body) env =
    let nuenv = Prelude.foldr (\x nuenv ->
                            H.insert x (eval body env) nuenv) env params
        in eval body nuenv

eval (AppExp e1 args) env = undefined
    -- let nuenv = Prelude.foldr (\y nuenv -> (eval y env) args
    --                         H.insert y (eval e1 env) nuenv) env args
    --     in eval args nuenv


eval (LetExp pairs body) env =
    let nuenv = Prelude.foldr (\(v,e) nuenv ->
                            H.insert v (eval e env) nuenv) env pairs
        in eval body nuenv


exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = do
   putStrLn $ show $ eval e env
   return (penv,env)
exec (SetStmt var e) penv env = do
   let val = eval e env
   return (penv, H.insert var val env)
exec p@(ProcedureStmt name args body) penv env =
    return (H.insert name p penv, env)
exec (CallStmt name args) penv env = undefined
    -- let Just (ProcedureStmt _ params body) = H.lookup name penv
        -- in exec args body penv env



repl :: PEnv -> Env -> [String] -> String -> Result
repl penv env [] _ =
  do putStr "> "
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return (penv,env)
        Right (LoadStmt fname) ->
           do putStrLn $ "Reading " ++ fname
              fdata <- readFile fname
              (nupenv,nuenv) <- repl penv env (lines fdata) fname
              repl nupenv nuenv [] ""
        Right x -> do (nupenv,nuenv) <- exec x penv env
                      repl nupenv nuenv [] "stdin"
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"
repl penv env (l:ll) fname =
   case parse stmt fname l of
      Right QuitStmt -> do putStrLn "File ended by quit."
                           return (penv,env)
      Right (LoadStmt fname') ->
         do putStrLn $ "Reading " ++ fname'
            fdata <- readFile fname'
            (nupenv,nuenv) <- repl penv env (lines fdata) fname'
            repl nupenv nuenv ll fname
      Right x -> do (nupenv,nuenv) <- exec x penv env
                    repl nupenv nuenv ll fname
      Left x -> do putStrLn $ show x
                   repl penv env [] "stdin" -- drop back to interactive


main = do
  putStrLn "Welcome to your interpreter!"
  repl H.empty H.empty [] "stdin"
