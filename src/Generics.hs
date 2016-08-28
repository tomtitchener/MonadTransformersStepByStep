{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
{--
http://dev.stephendiehl.com/hask/#generic-parsing
--}

module Main where

import Text.Parsec            ((<|>), string, try, many1, digit, char, letter, spaces)
import Text.Parsec.Text.Lazy  (Parser)
import Control.Applicative    ((<*), (*>), (<*>), (<$>), pure)
import GHC.Generics

class GParse f where
  gParse :: Parser (f a)

instance (Parse a) => GParse (K1 R a) where
  gParse = fmap K1 parse

instance (GParse f, Selector s) => GParse (M1 S s f) where
  gParse = fmap M1 gParse

-- Type synonym metadata for constructors
instance (GParse f, Constructor c) => GParse (C1 c f) where
  gParse =
    let con = conName (undefined :: t c f a) in
      (spaces >> string con >> spaces) *> (fmap M1 gParse)  

-- Constructor names
instance (Datatype d, GParse f) => GParse (D1 d f) where
  gParse = fmap M1 gParse

-- Sum types
instance (GParse a, GParse b) => GParse (a :+: b) where
  gParse = try (fmap L1 gParse) <|> try (fmap R1 gParse)

-- Product types
instance (GParse f, GParse g) => GParse (f :*: g) where
  gParse = (:*:) <$> try gParse <*> try gParse

-- Nullary constructors
instance GParse U1 where
  gParse = return U1

gparse :: (Generic g, GParse (Rep g)) => Parser g
gparse = fmap to gParse

class Parse a where
  parse :: Parser a
  default parse :: (Generic a, GParse (Rep a)) => Parser a
  parse = gparse

instance Parse Integer where
  parse = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Integer
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

instance Parse String where
   parse = many1 letter

data Scientist
  = Newton
  | Einstein
  | Schrodinger
  | Feynman
  deriving (Show, Generic, Parse)

type Name = String

data Musician
  = Vivaldi
  | Bach
  | Mozart
  | Beethoven
  | Musiker Name
  deriving (Show, Generic, Parse)

data Exp 
  = Lit Integer
  | Var Name
  | Plus Exp Exp 
  | App Exp Exp 
  | Abs Name Exp deriving (Show, Generic, Parse)

scientist :: Parser Scientist
scientist = parse

musician :: Parser Musician
musician = parse

expr :: Parser Exp
expr = parse

ip :: Parser Integer
ip = parse

sp :: Parser String
sp = parse

{--
λ: :set -XOverloadedStrings
λ: :m +Text.Parsec
λ: parseTest musician "Bach"
Bach
λ: parseTest scientist "Newton"
Newton
--}

main :: IO ()
main = undefined
