module Language.Mulang.Inspector.Typed (
  typesReturnAs,
  typesParameterAs,
  typesAs,
  declaresTypeSignature) where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Primitive


typesReturnAs :: IdentifierInspection
typesReturnAs predicate = containsDeclaration f
  where f (TypeSignature _ (Just _) name)  = predicate name
        f _                                = False

typesParameterAs :: IdentifierInspection
typesParameterAs predicate = containsDeclaration f
  where f (TypeSignature _ (Just names) _)  = any predicate names
        f _                                 = False

typesAs :: IdentifierInspection
typesAs predicate = containsDeclaration f
  where f (TypeSignature _ Nothing name) = predicate name
        f _                              = False

declaresTypeSignature :: IdentifierInspection
declaresTypeSignature = containsBoundDeclaration f
  where f (TypeSignature _ _ _) = True
        f _                     = False
