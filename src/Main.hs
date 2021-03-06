{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Data.Text.Lazy
import qualified Data.Map as Map
import GHC.Generics
import GParse
import System.Environment
import Text.ParserCombinators.Parsec

type Name = String

data Exp = Lit Integer | Var Name | Plus Exp Exp | Abs Name Exp | App Exp Exp deriving (Show, Generic, Parse)

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

evalVar :: Name -> Env -> Eval Value
evalVar name env = maybe (throwError ("unbound variable: " ++ name)) return (Map.lookup name env)

eval :: Exp -> Eval Value
eval (Lit i)      = tick >> return (IntVal i)
eval (Abs name e) = tick >> return (FunVal name e)
eval (Var name)   = tick >> tell [name] >> ask >>= evalVar name
eval (Plus e1 e2) = tick >> eval e1 >>= \v1-> eval e2 >>= \v2 -> evalPlus (v1, v2)
eval (App e1 e2)  = tick >> eval e1 >>= \v1-> eval e2 >>= \v2 -> evalApp (v1, v2)

parseExpStr :: Text -> Exp
parseExpStr str = either (error . show) id $ Text.ParserCombinators.Parsec.parse GParse.parse "parse" str

main :: IO ()
main = getArgs >>= mapM_ (print . runEval Map.empty 0 . eval . parseExpStr . pack)

{--
bash-3.2$ MonadTransformersStepByStep "Plus (Lit 12) (App (Abs x (Var x)) (Plus (Lit 4) (Lit 2)))"
((Right (IntVal 18),["x"]),8)
--}
