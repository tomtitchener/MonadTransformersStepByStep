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

-- Nullary constructors
instance GParse U1 where
  gParse = return U1

integer :: Parser Integer
integer = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Integer
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

-- start guessing
          
-- instance GParse (K1 R Integer) where
--   gParse = fmap K1 integer

-- instance GParse (K1 R String) where
--   gParse = fmap K1 (many1 letter)

instance (Parse a) => GParse (K1 R a) where
  gParse = fmap K1 parse
  
--   gParse = id
--     Couldn't match type ‘a0 -> a0’
--                    with ‘Text.Parsec.Prim.ParsecT
--                            text-1.2.2.1:Data.Text.Internal.Lazy.Text
--                            ()
--                            Data.Functor.Identity.Identity
--                            (K1 R Exp a)’
--     Expected type: Parser (K1 R Exp a)
--       Actual type: a0 -> a0
--     Relevant bindings include
--       gParse :: Parser (K1 R Exp a) (bound at src/Generics.hs:71:3)
--     Probable cause: ‘id’ is applied to too few arguments
--     In the expression: id
--     In an equation for ‘gParse’: gParse = id
-- Failed, modules loaded: none.
  
--gParse = fmap K1 gParse
--    Couldn't match type ‘f0 a0’ with ‘Exp’
--    Expected type: Text.Parsec.Prim.ParsecT
--                     text-1.2.2.1:Data.Text.Internal.Lazy.Text
--                     ()
--                     Data.Functor.Identity.Identity
--                     Exp
--      Actual type: Parser (f0 a0)
--    In the second argument of ‘fmap’, namely ‘gParse’
--    In the expression: fmap K1 gParse


-- gParse = fmap M1 gParse
--   Couldn't match type ‘M1 i0 c0 f0 p0’ with ‘K1 R Exp a’
--   Expected type: f0 p0 -> K1 R Exp a
--     Actual type: f0 p0 -> M1 i0 c0 f0 p0
--   Relevant bindings include
--     gParse :: Parser (K1 R Exp a) (bound at src/Generics.hs:71:3)
--   In the first argument of ‘fmap’, namely ‘M1’
--   In the expression: fmap M1 gParse

instance (GParse f, Selector s) => GParse (M1 S s f) where
  gParse = fmap M1 gParse
  
-- stop guessing

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

class Parse a where
  parse :: Parser a
  default parse :: (Generic a, GParse (Rep a)) => Parser a
  parse = gparse

instance Parse Integer where
   parse = integer

instance Parse String where
   parse = many1 letter

instance Parse Exp

instance Parse Scientist
   
instance Parse Musician

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
