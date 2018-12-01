{-# Language OverloadedStrings, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-} 
module Main where

import Lib

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

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
type PEnv = ([Statement], Env)
type Part = (Statement, Env)

lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing -> fail ("Unknown variable "++k)

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

eval (Not e0  ) = do evalb (const not) e0 (Const (B True)) 
                       where not2 a _ = not a -- hack, hack

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

type Run a = StateT Env (ExceptT String IO) a
runRun p = runExceptT $ runStateT p Map.empty

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\t -> ((), Map.insert s i t)) 

exec :: Statement -> Run ()
exec (Assign var e) = do
    current <- get
    case runEval current $ eval e of
        Right val -> set (var, val)
        Left e ->  liftIO $ System.print e

exec (If exp sa sb) = do
    current <- get
    case runEval current $ eval exp of
        Right (B val) -> exec sa
        Left e -> liftIO $ System.print e

exec (While exp s) = do
    current <- get
    case runEval current $ eval exp of
        Right (B val) -> exec s
        Left e -> liftIO $ System.print e

exec (Try t c) = catchError (exec t) (\e ->exec c)
exec (Print e) = do
    current <- get
    case runEval current $ eval e of
        Right val -> liftIO $ System.print val
        Left e -> liftIO $ System.print e

exec (Seq a b) = do
    exec a
    exec b
    
exec Pass = return ()

type Program = Writer Statement ()

instance Semigroup Statement where
    (<>) a b = a `Seq` b

instance Monoid Statement where
    mempty = Pass
    mappend a b = a `Seq` b

-- link together the program
build :: Program -> Statement
build p = snd . runIdentity $ runWriterT p

run :: Program -> IO()
run p = do
    result <- runExceptT $ (runStateT $ exec $ snd $ runIdentity $ (runWriterT p)) Map.empty
    case result of
        Right ( (), env ) -> return ()
        Left e -> System.print e

-- dsl for building a Program
infixl 1 .=
(.=) :: String -> Expr -> Program
var .= val = tell $ Assign var val

iif :: Expr -> Program -> Program -> Program
iif e y n = tell $ If e (build y) (build n)

while :: Expr -> Program -> Program
while e loop = tell $ While e (build loop)

print :: Expr -> Program
print e = tell $ Print e

main :: IO ()
main = do
        run $ do 
            "x" .=  (Const $ I 1)
            "x" .=  (Add (Var "x") (Var "x"))
            "x" .=  (Add (Var "x") (Var "x"))
            print $ Var "x"

