module Language.Mulang.Inspector.Primitive (
  containsExpression,
  containsDeclaration,
  containsBoundDeclaration,
  containsBody,
  matchesType,
  Inspection,
  IdentifierInspection) where

import           Language.Mulang.Ast
import           Language.Mulang.Identifier (IdentifierPredicate)
import           Language.Mulang.Generator (expressions, boundDeclarations, equationBodies, declarations)

type Inspection = Expression  -> Bool
type IdentifierInspection = IdentifierPredicate -> Inspection

containsExpression :: (Expression -> Bool) -> Inspection
containsExpression f = has f expressions

containsBody :: (EquationBody -> Bool)-> Inspection
containsBody f = has f equationBodies

containsBoundDeclaration :: (Expression -> Bool) -> IdentifierInspection
containsBoundDeclaration f b  = has f (boundDeclarations b)

containsDeclaration :: (Expression -> Bool) -> Inspection
containsDeclaration f = has f (map snd . declarations)

matchesType :: IdentifierPredicate -> Pattern -> Bool
matchesType predicate (TypePattern n)               = predicate n
matchesType predicate (AsPattern _ (TypePattern n)) = predicate n
matchesType _         _                             = False

-- private

has f g = any f . g
