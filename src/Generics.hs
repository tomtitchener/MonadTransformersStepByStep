{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{--
http://dev.stephendiehl.com/hask/#generic-parsing
--}

module Main where

import Text.Parsec
import Text.Parsec.Text.Lazy
import Control.Applicative ((<*), (<*>), (<$>))
import GHC.Generics

class GParse f where
  gParse :: Parser (f a)

-- Type synonym metadata for constructors
instance (GParse f, Constructor c) => GParse (C1 c f) where
  gParse =
    let con = conName (undefined :: t c f a) in
    (fmap M1 gParse) <* string con

-- Constructor names
instance GParse f => GParse (D1 c f) where
  gParse = fmap M1 gParse

-- Sum types
instance (GParse a, GParse b) => GParse (a :+: b) where
  gParse = try (fmap L1 gParse <|> fmap R1 gParse)

-- Product types
instance (GParse f, GParse g) => GParse (f :*: g) where
  gParse = (:*:) <$> gParse <*> gParse

-- Nullary constructors
instance GParse U1 where
  gParse = return U1

data Scientist
  = Newton
  | Einstein
  | Schrodinger
  | Feynman
  deriving (Show, Generic)

data Musician
  = Vivaldi
  | Bach
  | Mozart
  | Beethoven
  deriving (Show, Generic)

type Name = String

data Exp 
  = Lit Integer 
  | Var Name
  | Plus Exp Exp 
  | Abs Name Exp 
  | App Exp Exp deriving (Show, Generic)

{--
/Users/Tom/Documents/Dev/Haskell/MonadTransformersStepByStep/src/Generics.hs:77:7:
    No instance for (GParse (M1 S NoSelector (Rec0 Integer)))
      arising from a use of ‘gparse’
    In the expression: gparse
    In an equation for ‘exp’: exp = gparse
--}

gparse :: (Generic g, GParse (Rep g)) => Parser g
gparse = fmap to gParse

scientist :: Parser Scientist
scientist = gparse

musician :: Parser Musician
musician = gparse

exp :: Parser Exp
exp = gparse

{--
λ: :set -XOverloadedStrings
λ: parseTest musician "Bach"
Bach
λ: parseTest scientist "Newton"
Newton
--}

main :: IO ()
main = undefined