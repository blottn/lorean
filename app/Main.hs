{-# Language OverloadedStrings, FlexibleContexts #-} 

--overloaded strings for ease of use
-- Flexible Contents for throwError in loopup k t

module Main where

import Lib

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe
import Data.Either

-- for reading the user input 
import qualified Text.Read as Reader
import qualified System.IO as System
import qualified System.Environment as Environment
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
data Val = I Int | B Bool
           deriving (Eq, Show, Read)

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr | Not Expr 
     | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
     | Var String
   deriving (Eq, Show, Read)

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
set (s,i) = modify $ assign (s, i)

-- primitive statements
exec :: Statement -> Instruction -> Run ()
exec (Assign var e) i  = do
    current <- gets $ getE
    case runEval current $ eval e of
        Right val -> set (var, val)
        Left e ->  throwError e

exec (If exp sa sb) i = do
    current <- gets $ getE
    case runEval current $ eval exp of
        Right (B True) -> exec sa i
        Right (B False) -> exec sb i
        Left e -> throwError e

exec (Print e) i = do
    current <- gets $ getE
    case runEval current $ eval e of
        Right val -> liftIO $ System.print val
        Left e -> throwError e

exec Pass _ = return ()

type Stage = (Env, Statement)
type Breakpoint = Expr  -- alias that thingy
type PState = ([Stage], Env, Instruction, [Breakpoint])

-- state functions
getE :: PState -> Env
getE (_,env,_,_) = env

getI :: PState -> Instruction
getI (_,_,instruction,_) = instruction

getB :: PState -> [Breakpoint]
getB (_,_,_,bs) = bs

setInst :: Instruction -> PState -> PState
setInst i (h,e,_,b) = (h,e,i,b)

assign :: (Name, Val) -> PState -> PState
assign  (k, v) (h,e,i,b) = (h, Map.insert k v $ e, i, b)

addBreak :: Breakpoint -> PState -> PState
addBreak bp (h,e,i,[]) = (h,e,i,[bp])
addBreak bp (h,e,i,bs) = (h,e,i,bp:bs)

addStage :: Stage -> PState -> PState
addStage stage (hs,e,i,b) = (stage:hs, e, i, b)

rewind :: PState -> PState
rewind ((h:[]),e,i,b) = (h:[],e,i,b)
rewind ((h:hs),e,i,b) = (hs, e, i, b)

logState :: Statement -> Run ()
logState s = do
    e <- gets $ getE
    modify $ addStage (e,s)

data Instruction = Br Expr | Fr | Ba | Go  deriving (Read, Show)

valJoiner :: Bool -> Val -> Bool
valJoiner True (B _) = True
valJoiner _ (B True) = True
valJoiner _ _ = False

getNextInst :: Instruction -> Run ()
getNextInst Go = do
    bps <- gets $ getB
    env <- gets $ getE
    case foldl valJoiner False $ rights $ map ((runEval env) . eval) bps of
        True -> getNextInst Fr
        _ -> return ()

getNextInst i = do
    state <- get
    liftIO $ putStrLn ( "State > " ++ (show state))
    inp <- liftIO $ Reader.readMaybe <$> getLine
    case inp of
        Just (Br b) -> (modify $ addBreak b) >> getNextInst i
        Just inst -> modify $ setInst inst
        Nothing -> (liftIO $ putStrLn eMsg) >> getNextInst i

getBreak :: Run ()
getBreak = do
    b <- liftIO $ Reader.readMaybe <$> getLine
    case b of
        Just br -> modify $ addBreak br
        Nothing -> return ()

type Run a = StateT PState (ExceptT String IO) a    -- monad in which a program is run
runRun p = runExceptT $ runStateT p Map.empty

-- useful for linking together a program
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

reverseState :: Run()
reverseState = modify $ rewind

getStatement :: PState -> Statement
getStatement (xs,_,_,_) = snd $ head xs

interpret :: Statement -> Run ()
interpret (Seq a b) = do
    cInst <- gets $ getI
    case cInst of
        Ba -> do 
                reverseState
                nex <- gets $ getStatement
                interpret nex
        _ -> do 
                interpret a
                interpret b
        

interpret (While ex stat) = do
    current <- gets $ getE
    cInst <- gets $ getI
    case cInst of
        Ba -> return ()
        Fr -> logState (While ex stat)
        Go -> logState (While ex stat)
        Br _ -> logState (While ex stat)

    liftIO $ putStrLn $ show (While ex stat)
    getNextInst cInst

    nInst <- gets $ getI
    case nInst of
        Ba -> do
                reverseState
                nex <- gets $ getStatement
                interpret nex
        _ -> case runEval current $ eval ex of
                Right (B True) -> interpret stat >> interpret (While ex stat)
                Right (B False) -> return ()
                Left e -> liftIO $ System.print e

interpret (If ex a b) = do
    current <- gets $ getE
    cInst <- gets $ getI
    case cInst of
        Ba -> return ()
        Fr -> logState (If ex a b)
        Go -> logState (If ex a b)
        Br _ -> logState (If ex a b)

    liftIO $ putStrLn $ show (If ex a b)
    getNextInst cInst

    nInst <- gets $ getI
    case nInst of
        Ba -> do
                reverseState
                nex <- gets $ getStatement
                interpret nex
        _ -> case runEval current $ eval ex of
                Right (B True) -> interpret a
                Right (B False) -> interpret b
                Left e -> liftIO $ System.print e

interpret (Try a b) = do
    cInst <- gets $ getI
    case cInst of
        Ba -> return ()
        Fr -> logState (Try a b)
        Go -> logState (Try a b)
        Br _ -> logState (Try a b)

    liftIO $ putStrLn $ show (Try a b)
    getNextInst cInst


    nInst <- gets $ getI
    case nInst of
        Ba -> do
                reverseState
                nex <- gets $ getStatement
                interpret nex
        _ -> (interpret a) `catchError` (\e -> do 
                                        (liftIO $ putStrLn e)
                                        interpret b)

interpret s = do
    liftIO $ putStrLn $ show s
    cInst <- gets $ getI
    case cInst of
        Ba -> return ()
        Fr -> logState s
        Go -> logState s
        Br _ -> logState s

    getNextInst cInst
    
    nInst <- gets $ getI
    case nInst of
        Ba -> do
                reverseState
                nex <- gets $ getStatement
                interpret nex
        _ -> exec s cInst

-- dsl for building a Program handy for debug
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

--readP (x:xs) = readFile x

main :: IO ()
main = do
 --       args <- Environment.getArgs
 --       p <- readP args
 --       case Reader.readMaybe $ p of
  --          Just d -> putStrLn "success"
  --          Nothing -> putStrLn "compilation error"
      --  pdata <- Reader.readMaybe $ readFile $ f
--        case pdata of
 --           Just d -> build pdata
  --          Nothing -> putStrLn "invalid file/ no data"
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

