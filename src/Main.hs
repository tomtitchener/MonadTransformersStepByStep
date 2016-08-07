{--
Monad Transformers Step By Step, Martin Grabmuller,
  - https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf
--}

module Main where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer | Var Name | Plus Exp Exp | Abs Name Exp | App Exp Exp deriving (Show)

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

type Env = Map.Map Name Value

data Value = IntVal Integer | FunVal Env Name Exp deriving (Show)

type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a

runEval :: Env -> Integer -> Eval a -> ((Either String a, [String]), Integer)
runEval env st ev = runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

tick :: (Num s, MonadState s m) => m ()
tick = get >>= \i -> put (i+1)

eval :: Exp -> Eval Value
eval (Lit i) = tick >> return (IntVal i)
eval (Var n) = tick >> tell [n] >> ask >>= \env -> maybe (throwError ("unbound variable: " ++ n)) return (Map.lookup n env)
eval (Abs n e) = tick >> ask >>= \env -> return $ FunVal env n e
eval (Plus e1 e2) = do
  tick
  e1' <- eval e1
  e2' <- eval e2
  case (e1', e2') of
   (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
   _ -> throwError ("type error in addition of expressions e1 " ++ show e1 ++ " or e2 " ++ show e2)
eval (App e1 e2) = do
  val1 <- eval e1
  val2 <- eval e2
  case val1 of
   FunVal env' n body -> local (const (Map.insert n val2 env')) (eval body)
   _ -> throwError ("type error in application of expression " ++ show e1 ++ " to value " ++ show val1)

{--
λ: runEval Map.empty 0 (eval exampleExp)
((Right (IntVal 18),["x"]),7)
--}

main :: IO ()
main = undefined
