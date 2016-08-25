{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

{--
http://dev.stephendiehl.com/hask/#generic-parsing
--}

module Main where

import Text.Parsec            ((<|>), string, try, many1, digit, char)
import Text.Parsec.Text.Lazy  (Parser)
import Control.Applicative    ((<*), (*>), (<*>), (<$>))
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

-- Nullary constructors
instance GParse U1 where
  gParse = return U1

--instance GParse (Rep f) => GParse (K1 R f) where
--  gParse = undefined

integer :: Parser Integer
integer = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Integer
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

instance GParse (K1 R Integer) where
  gParse = undefined

instance GParse (K1 R String) where
  gParse = undefined

instance GParse (K1 R Exp) where
  gParse = undefined

instance (GParse f, Selector s) => GParse (M1 S s f) where
  gParse = undefined

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

data ScientistAndMusician
  = ScientistAndMusician Scientist Musician
  deriving (Show, Generic)

type Name = String

data Exp 
  = Lit Integer
  | Var String
  | Plus Exp Exp 
  | Abs Name Exp 
  | App Exp Exp deriving (Show, Generic)

{--
*Main> :kind! Rep Exp
Rep Exp :: * -> *
= D1
    Main.D1Exp
    ((C1 Main.C1_0Exp (S1 NoSelector (Rec0 Integer))
      :+: C1 Main.C1_1Exp (S1 NoSelector (Rec0 String)))
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

gparse :: (Generic g, GParse (Rep g)) => Parser g
gparse = fmap to gParse

scientist :: Parser Scientist
scientist = gparse

musician :: Parser Musician
musician = gparse

exp :: Parser Exp
exp = gparse

{--
/Users/tom_titchener/Documents/Dev/Haskell/MonadTransformersStepByStep/src/Generics.hs:96:7:
    No instance for (GParse (Rep Integer))
      arising from a use of ‘gparse’
    In the expression: gparse
    In an equation for ‘exp’: exp = gparse
--}


{--
λ: :set -XOverloadedStrings
λ: parseTest musician "Bach"
Bach
λ: parseTest scientist "Newton"
Newton
--}

main :: IO ()
main = undefined
