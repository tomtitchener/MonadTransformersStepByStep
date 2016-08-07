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

type Env = Map.Map Name Value

data Value = IntVal Integer | FunVal Name Exp deriving (Show)

type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a

runEval :: Env -> Integer -> Eval a -> ((Either String a, [String]), Integer)
runEval env st ev = runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

-- count evals
tick :: (Num s, MonadState s m) => m ()
tick = get >>= \i -> put (i+1)

-- helpers
evalPlus :: (Value, Value) -> Eval Value
evalPlus (IntVal i1, IntVal i2) = return $ IntVal (i1 + i2)
evalPlus (v1, v2)               = throwError ("type error in addition of values v1 " ++ show v1 ++ " or v1 " ++ show v2)

evalApp :: (Value, Value) -> Eval Value
evalApp (FunVal name body, v2) = local (Map.insert name v2) (eval body)
evalApp (v1, v2)               = throwError ("type error in application of value " ++ show v1 ++ " to value " ++ show v2)

eval :: Exp -> Eval Value
eval (Lit i)      = tick >> return (IntVal i)
eval (Var n)      = tick >> tell [n] >> ask >>= \env -> maybe (throwError ("unbound variable: " ++ n)) return (Map.lookup n env)
eval (Abs n e)    = tick >> return (FunVal n e)
eval (Plus e1 e2) = tick >> eval e1 >>= \v1-> eval e2 >>= \v2 -> evalPlus (v1, v2)
eval (App e1 e2)  = tick >> eval e1 >>= \v1-> eval e2 >>= \v2 -> evalApp (v1, v2)

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)

{--
λ: runEval Map.empty 0 (eval exampleExp)
((Right (IntVal 18),["x"]),8)
--}

main :: IO ()
main = undefined
