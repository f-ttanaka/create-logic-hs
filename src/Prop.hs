module Prop where

import Common

data Prop
  = Var String
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
  deriving (Eq, Show)

toNNF :: Prop -> Prop
toNNF (Imply a b) = toNNF (Or (Not a) b)
toNNF (Not (Imply a b)) = toNNF (And a (Not b))
toNNF (Not (Not a)) = toNNF a
toNNF (Not (And a b)) = toNNF (Or (Not a) (Not b))
toNNF (Not (Or a b)) = toNNF (And (Not a) (Not b))
toNNF (And a b) = And (toNNF a) (toNNF b)
toNNF (Or a b) = Or (toNNF a) (toNNF b)
toNNF (Not a) = Not (toNNF a)
toNNF v@(Var _) = v

negationOf :: Prop -> Prop
negationOf (Not p) = p
negationOf p = Not p

isLiteral :: Prop -> Bool
isLiteral (Var _) = True
isLiteral (Not (Var _)) = True
isLiteral _ = False
