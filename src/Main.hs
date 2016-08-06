{--
Monad Transformers Step By Step, Martin Grabmuller,
  - https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf
--}

module Main where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer | Var Name | Plus Exp Exp | Abs Name Exp | App Exp Exp deriving (Show)

type Env = Map.Map Name Value

data Value = IntVal Integer | FunVal Env Name Exp deriving (Show)

type Eval a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a

runEval :: Env -> Integer -> Eval a -> ((Either String a, [String]), Integer)
runEval env st ev = runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval :: Exp -> Eval Value

eval (Lit i) = do
  tick
  return $ IntVal i

eval (Var n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
   Nothing -> throwError ("unbound variable: " ++ n)
   Just val -> return val

eval (Plus e1 e2) = do
  tick
  e1' <- eval e1
  e2' <- eval e2
  case (e1', e2') of
   (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
   _ -> throwError ("type error in addition of expressions e1 " ++ show e1 ++ " or e2 " ++ show e2)

eval (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e

eval (App e1 e2) = do
  val1 <- eval e1
  val2 <- eval e2
  case val1 of
   FunVal env' n body -> local (const (Map.insert n val2 env')) (eval body)
   _ -> throwError ("type error in application of expression " ++ show e1 ++ " to value " ++ show val1)

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

{--
λ: runEval Map.empty 0 (eval exampleExp)
((Right (IntVal 18),["x"]),7)
--}

main :: IO ()
main = undefined

{--
src/Main.hs:11:1: Warning:
    Module ‘Control.Monad.Error’ is deprecated:
      Use Control.Monad.Except instead

src/Main.hs:26:28: Warning:
    In the use of type constructor or class ‘ErrorT’
    (imported from Control.Monad.Error, but defined in transformers-0.4.2.0:Control.Monad.Trans.Error):
    Deprecated: "Use Control.Monad.Trans.Except instead"

src/Main.hs:29:57: Warning:
    In the use of ‘runErrorT’
    (imported from Control.Monad.Error, but defined in transformers-0.4.2.0:Control.Monad.Trans.Error):
    Deprecated: "Use Control.Monad.Trans.Except instead"
--}
