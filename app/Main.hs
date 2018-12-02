{-# Language OverloadedStrings,NoMonomorphismRestriction, FlexibleContexts #-} 
--overloaded strings for ease of use
-- nomorphism.. for the deriving show in Instruction data
-- Flexible Contents for throwError in loopup k t

module Main where

import Lib

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

-- for reading the user input 
import qualified Text.Read as Reader
import qualified System.IO as System

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
data Val = I Int | B Bool
           deriving (Eq, Show)

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr | Not Expr 
     | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
     | Var String
   deriving (Eq, Show)

type Name = String 
type Env = Map.Map Name Val

lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing -> throwError ("Unknown variable "++k)

type Eval a = ReaderT Env (ExceptT String Identity) a 

runEval env ex = runIdentity ( runExceptT ( runReaderT ex env))

-- need to add div by 0 error TODO
evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> throwError "type error in arithmetic expression"

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> throwError "type error in boolean expression"


evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> throwError "type error in arithmetic expression"

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (const not) (Const (B True)) e0

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1
                        
eval (Var s) = do env <- ask
                  lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}

data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass                    
      deriving (Eq, Show)

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\t -> ( (), ([], (Map.insert s i $ getEnv t), Fr, []))) 

exec :: Statement -> Instruction -> Run ()
exec (Assign var e) i  = do
    current <- gets $ getEnv
    case runEval current $ eval e of
        Right val -> set (var, val)
        Left e ->  throwError e

exec (If exp sa sb) i = do
    current <- gets $ getEnv
    case runEval current $ eval exp of
        Right (B True) -> exec sa i
        Right (B False) -> exec sb i
        Left e -> throwError e

exec (Print e) i = do
    current <- gets $ getEnv
    case runEval current $ eval e of
        Right val -> liftIO $ System.print val
        Left e -> throwError e

exec Pass _ = return ()


type Stage = (Env, Statement)
type Breakpoint = Expr  -- alias that thingy
type PState = ([Stage], Env, Instruction, [Breakpoint])

-- state functions
getEnv :: PState -> Env
getEnv (_,env,_,_) = env

setInst :: Instruction -> PState -> PState
setInst i (h,e,_,b) = (h,e,i,b)

data Instruction =  Fr | Ba | Go  deriving (Read, Show)

type Run a = StateT PState (ExceptT String IO) a    -- monad in which a program is run
runRun p = runExceptT $ runStateT p Map.empty

type Program = Writer Statement ()

instance Semigroup Statement where
    (<>) a b = a `Seq` b

instance Monoid Statement where
    mempty = Pass
    mappend a b = a `Seq` b

-- link together the program
build :: Program -> Statement
build p = snd . runIdentity $ runWriterT p

-- actual interpreter part
-- initial entry point
run :: Statement -> IO()
run p = do
    result <- runExceptT $ (runStateT $ interpret p) ([], Map.empty, Fr,[])
    case result of
        Right ( (), env ) -> return ()
        Left e -> System.print e

eMsg :: String
eMsg = "Error, invalid input. Valid inputs are: Fr, Ba, Go, Br <Expr>"

getNextInst :: Statement -> Run ()
getNextInst s = do
    inp <- liftIO $ Reader.readMaybe <$> getLine
    case inp of
        Just inst -> modify $ setInst inst
        Nothing -> (liftIO $ putStrLn eMsg) >> getNextInst s

interpret :: Statement -> Run ()
interpret (Seq a b) = do
    interpret a
    interpret b

interpret (While ex stat) = do
    current <- gets $ getEnv
    liftIO $ putStrLn $ show (While ex stat)
    getNextInst (While ex stat)
    case runEval current $ eval ex of
        Right (B True) -> interpret stat >> interpret (While ex stat)
        Right (B False) -> return ()
        Left e -> liftIO $ System.print e

interpret (If ex a b) = do
    current <- gets $ getEnv
    liftIO $ putStrLn $ show (If ex a b)
    getNextInst (If ex a b)
    case runEval current $ eval ex of
        Right (B True) -> interpret a
        Right (B False) -> interpret b
        Left e -> liftIO $ System.print e

interpret (Try a b) = do
    liftIO $ putStrLn $ show (Try a b)
    getNextInst (Try a b)
    (interpret a) `catchError` (\e -> do 
                                        (liftIO $ putStrLn e)
                                        interpret b)

interpret s = do
    liftIO $ putStrLn $ show s
    inp <- liftIO $ Reader.readMaybe <$> getLine
    case inp of
        Just inst -> exec s inst
        Nothing -> do
                    liftIO $ putStrLn eMsg
                    interpret s

-- dsl for building a Program
infixl 1 .=
(.=) :: String -> Expr -> Program
var .= val = tell $ Assign var val

iif :: Expr -> Program -> Program -> Program
iif e y n = tell $ If e (build y) (build n)

while :: Expr -> Program -> Program
while e loop = tell $ While e (build loop)

try :: Program -> Program -> Program
try p1 p2 = tell $ Try (build p1) (build p2)

print :: Expr -> Program
print e = tell $ Print e

main :: IO ()
main = do
        run $ build $ do 
            print $ (Const $ I 1)
            "x" .=  (Const $ I 1)
            try ("x" .= (Div (Var "w") (Const $ I 1)))(print $ (Const $ I 100))
            iif (Gt (Var "x") (Const $ I 6)) (do
                "x" .=  (Add (Var "x") (Const $ I 1))
                "x" .=  (Add (Var "x") (Const $ I 1)))
                (do
                    print $ ((Const $ I 33)))
            "x" .=  (Add (Var "x") (Var "x"))
            print $ Var "x"

