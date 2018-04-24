module GraphQL.Resolver where

import Protolude

import Data.Row
       (type (.+), KnownSymbol, Label(..), Rec, Row, (.+), (.==))
import Data.Row.Poly (type (.==))
import qualified Data.Row.Poly as Row (Map)
import GHC.Types (Symbol, Type)
import qualified GraphQL.Language.AST as AST
import GraphQL.Type

data SchemaResolver (m :: Type -> Type) (schema :: Row TypeDefinition) where
  SchemaResolver
    :: Rec (Row.Map (TypeResolver m schema) schema) -> SchemaResolver m schema

type Resolver (m :: Type -> Type) (schema :: Row TypeDefinition) (entries :: Row TypeDefinition)
   = SchemaResolver m schema -> Rec (Row.Map (TypeResolver m schema) entries)

data TypeResolver (m :: Type -> Type) (schema :: Row TypeDefinition) (definition :: TypeDefinition) where
  TypeResolverScalar
    :: ScalarResolver scalar
    -> TypeResolver m schema ('SCALAR scalar description)
  TypeResolverObject
    :: InterfacesSatisfied schema interfaces fields
    => ObjectResolver m schema object fields
    -> TypeResolver m schema ('OBJECT object description interfaces fields)

class InterfaceSatisfied (schema :: Row TypeDefinition) (interface :: Symbol) (fields :: Row FieldDefinition)

class InterfacesSatisfied (schema :: Row TypeDefinition) (interfaces :: [Symbol]) (fields :: Row FieldDefinition)

instance InterfacesSatisfied schema '[] fields

instance ( InterfaceSatisfied schema interface fields
         , InterfacesSatisfied schema interfaces fields
         ) =>
         InterfacesSatisfied schema (interface ': interfaces) fields

data ScalarResolver scalar = ScalarResolver
  { serialize :: scalar -> AST.Value
  , deserialize :: AST.Value -> Maybe scalar
  }

newtype ObjectResolver (m :: Type -> Type) (schema :: Row TypeDefinition) (object :: Type) (fields :: Row FieldDefinition) = ObjectResolver
  { fieldResolvers :: Rec (Row.Map (FieldResolver m schema object) fields)
  }

newtype FieldResolver (m :: Type -> Type) (schema :: Row TypeDefinition) (context :: Type) (field :: FieldDefinition) = FieldResolver
  { resolver :: FieldContext context -> m AST.Value
  }

data ExecutionContext (value :: Type) = ObjectContext
  { contextValue :: value
  , typename :: Text
  , selectionSet :: AST.SelectionSet
  , fragmentDefinitions :: Map AST.Name AST.FragmentDefinition
  }

data FieldContext (context :: Type) = FieldContext
  { context :: context
  , arguments :: [AST.Argument]
  , directives :: [AST.Directive]
  , selectionSet :: AST.SelectionSet
  }

-- syntactic sugar: this should be moved to a DSL module
class Definable (m :: Type -> Type) (schema :: Row TypeDefinition) (def :: TypeDefinition) where
  type Spec m schema def :: Type
  toDefinition ::
       Spec m schema def -> SchemaResolver m schema -> TypeResolver m schema def

instance Definable m schema ('SCALAR scalar description) where
  type Spec m schema ('SCALAR scalar description) = ScalarResolver scalar
  toDefinition = const . TypeResolverScalar

instance InterfacesSatisfied schema interfaces fields =>
         Definable m schema ('OBJECT object description interfaces fields) where
  type Spec m schema ('OBJECT object description interfaces fields) = SchemaResolver m schema -> ObjectResolver m schema object fields
  toDefinition resolver schema = TypeResolverObject (resolver schema)

define ::
     forall label m schema def. (KnownSymbol label, Definable m schema def)
  => Spec m schema def
  -> Resolver m schema (label .== def)
define resolver schema = Label .== toDefinition resolver schema

combine :: Applicative m => m (Rec r1) -> m (Rec r2) -> m (Rec (r1 .+ r2))
combine = liftA2 (.+)
