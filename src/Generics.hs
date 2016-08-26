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

import Debug.Trace

import Text.Parsec            ((<|>), string, try, many1, digit, char, letter)
import Text.Parsec.Text.Lazy  (Parser)
import Control.Applicative    ((<*), (*>), (<*>), (<$>), pure)
import GHC.Generics

-- Parser is defined by Text.Parsec.Text.Lazy as type Parser = Parsec Text ().
-- Parsec is defined by Text.Parsec as type Parsec s u = ParsecT s u Identity
-- ParsecT is defined by Text.Parsec as data ParsecT s u m a where ...
-- "ParsecT s u m a is a parser with stream type s, user state type u,
-- underlying monad m and return type a."
-- So type Parser = Parsec Text () yields stream type Text, user state Unit,
-- monad Identity, and return type TBD, and Parser (f a)
-- If we load this file in ghci and say :info GParse, the output starts out with:
-- "class GParse (f :: * -> *) where" so you can see the kind for f is * -> *,
-- same as the type for m in Monad:
-- "class Applicative m => Monad (m :: * -> *) where"
-- Another example is Maybe:
-- "*Main> :kind! Maybe
--  Maybe :: * -> *"
-- So in GParse, 'f' is a type constructor that takes another type as an argument
-- and the single GParse class method gParse gives a Parser of return type (f a).
--
-- Note this example doesn't go the distance and make a type class you can 
-- derive automatically the way the example in Menu.hs does.  You can't derive
-- a GParse dirction because of the kind of f is * -> * instead of just *, 
-- which is where the Menu/GMenu layering happens in Menu.hs.
--
-- Another entirely different strategy would be to model Gonzalez's
-- Haskell Server Generic library where he doesn't mess with any of
-- Text.Parsec and instead hand-rolls his own Parser type with default
-- implementations for anything with a Read instance.  One hitch is
-- his parser is explicilty for his "Route" type only.
--
class GParse f where
  gParse :: Parser (f a)

-- start guessing
instance (Parse a) => GParse (K1 R a) where
  gParse = trace "GParse (K1 R a)" $ fmap K1 parse

-- Almost certainly something goes wrong here.
-- 
instance (GParse f, Selector s) => GParse (M1 S s f) where
  gParse = trace "GParse (M1 S  s f)" $  fmap M1 gParse

-- stop guessing

-- Type synonym metadata for constructors
-- Could this be where things go wrong?
-- In Diehl's types, the constructors take no arguments.
-- In the Exp type they do.  Can I find an example of
-- Generic code that matches on (C1 c f) to see how it
-- treats constructors with values?  Does this recur?
-- We do get this far, because if I just put an error
-- in the execution line I hit it right away.  But
-- testing for conIsRecord doesn't do anything, it's
-- never true.
-- Note that in comparison of traces between parsing
-- Diehl's examples vs. parsing an Exp, there's always
-- a "GParse U1" trace that shows us bottoming out for
-- Scientist or Musician, but never for Exp.
instance (GParse f, Constructor c) => GParse (C1 c f) where
  gParse =
    let con = trace "GParse (C1 c f)" $ conName (undefined :: t c f a) in
      (fmap M1 gParse) <* (trace $ "con name " ++ con ++ "!") string con

-- Constructor names
instance GParse f => GParse (D1 c f) where
  gParse = trace "GParse (D1 c f)" $ fmap M1 gParse

-- Sum types
instance (GParse a, GParse b) => GParse (a :+: b) where
  gParse = trace "GParse (a :+: b)" $  try (fmap L1 gParse <|> fmap R1 gParse)

-- Product types
instance (GParse f, GParse g) => GParse (f :*: g) where
  gParse = trace "GParse (f :*: g)" $ (:*:) <$> gParse <*> gParse

-- Nullary constructors
instance GParse U1 where
  gParse = trace "GParse U1" $ return U1

gparse :: (Generic g, GParse (Rep g)) => Parser g
gparse = fmap to gParse

class Parse a where
  parse :: Parser a
  default parse :: (Generic a, GParse (Rep a)) => Parser a
  parse = gparse

instance Parse Integer where
  parse = trace "Parse Integer" $ rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Integer
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

instance Parse String where
   parse = trace "Parse String" $ many1 letter

data Scientist
  = Newton
  | Einstein
  | Schrodinger
  | Feynman
  deriving (Show, Generic, Parse)

data Musician
  = Vivaldi
  | Bach
  | Mozart
  | Beethoven
  deriving (Show, Generic, Parse)

type Name = String

data Exp 
  = Lit Integer
  | Var Name
  | Plus Exp Exp 
  | Abs Name Exp 
  | App Exp Exp deriving (Show, Generic, Parse)

{--
*Main> :kind! Rep Exp
Rep Exp :: * -> *
= D1
    Main.D1Exp
    ((C1 Main.C1_0Exp (S1 NoSelector (Rec0 Integer))
      :+: C1 Main.C1_1Exp (S1 NoSelector (Rec0 String)))A
     :+: (C1
            Main.C1_2Exp
            (S1 NoSelector (Rec0 Exp) :*: S1 NoSelector (Rec0 Exp))
          :+: (C1
                 Main.C1_3Exp
                 (S1 NoSelector (Rec0 Name) :*: S1 NoSelector (Rec0 Exp))
               :+: C1
                     Main.C1_4Exp
                     (S1 NoSelector (Rec0 Exp) :*: S1 NoSelector (Rec0 Exp)))))
--}


scientist :: Parser Scientist
scientist = parse

musician :: Parser Musician
musician = parse

expr :: Parser Exp
expr = parse

{--
/Users/tom_titchener/Documents/Dev/Haskell/MonadTransformersStepByStep/src/Generics.hs:96:7:
    No instance for (GParse (Rep Integer))
      arising from a use of ‘gparse’
    In the expression: gparse
    In an equation for ‘exp’: exp = gparse
--}


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
