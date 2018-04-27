{-# LANGUAGE UndecidableInstances #-}

module GraphQL.Type where

import Data.Row (Rec, Var, type (.+), type (.==))
import Data.Row.Poly (Empty, Ifte, LT(..), Row(..))
import GHC.Types (Symbol, Type)
import qualified GHC.TypeLits as TL
import Protolude

-- schema representation
data TypeRef where
  NULLABLE :: Symbol -> TypeRef
  NULLABLE_LIST_OF :: TypeRef -> TypeRef
  NON_NULL :: Symbol -> TypeRef
  NON_NULL_LIST_OF :: TypeRef -> TypeRef

type Name = Symbol

type Description = Symbol

type DeprecationReason = Symbol

type Interface = Symbol

data TypeDefinition where
  SCALAR
    :: Type
    -> Description
    -> TypeDefinition
  OBJECT
    :: Type
    -> Description
    -> [Interface]
    -> Row FieldDefinition
    -> TypeDefinition
  INTERFACE
    :: Description
    -> Row FieldDefinition
    -> TypeDefinition
  UNION
    :: Description
    -> [Name]
    -> TypeDefinition
  ENUM
    :: Description
    -> Row EnumValueDefinition
    -> TypeDefinition
  INPUT_OBJECT
    :: Description
    -> Row InputValueDefinition
    -> TypeDefinition

data FieldDefinition where
  FIELD
    :: Row ArgumentDefinition
    -> TypeRef
    -> Description
    -> FieldDefinition
  DEPRECATED_FIELD
    :: Row ArgumentDefinition
    -> TypeRef
    -> DeprecationReason
    -> Description
    -> FieldDefinition

type ArgumentDefinition = InputValueDefinition

data InputValueDefinition where
  INPUT_VALUE
    :: TypeRef
    -> Description
    -> InputValueDefinition
  INPUT_VALUE_WITH_DEFAULT
    :: TypeRef
    -> Symbol
    -> Description
    -> InputValueDefinition

data EnumValueDefinition where
  ENUM_VALUE
    :: Description
    -> EnumValueDefinition
  DEPRECATED_ENUM_VALUE
    :: DeprecationReason
    -> Description
    -> EnumValueDefinition

-- type families to determine corresponding types

type family GetType (schema :: Row TypeDefinition) (ref :: TypeRef) :: Type where
  GetType schema ('NON_NULL symbol) = GetNamedType schema symbol
  GetType schema ('NON_NULL_LIST_OF ref) = [GetType schema ref]
  GetType schema ('NULLABLE symbol) = Maybe (GetNamedType schema symbol)
  GetType schema ('NULLABLE_LIST_OF ref) = Maybe [GetType schema ref]

type family GetNamedType (schema :: Row TypeDefinition) (name :: Symbol) :: Type where
  GetNamedType ('R schema) name = GetNamedTypeR schema name schema

-- these type families peek directly into the `Row` structure to improve typechecking performance slightly
type family GetNamedTypeR (fullSchema :: [LT TypeDefinition]) (name :: Symbol) (definitions :: [LT TypeDefinition]) :: Type where
  GetNamedTypeR _ name '[] =
    TL.TypeError (TL.Text "No GraphQL definition found for " TL.:<>: TL.ShowType name)
  GetNamedTypeR _ name (name ':-> 'SCALAR scalar _ ': _) =
    scalar
  GetNamedTypeR _ name (name ':-> 'OBJECT object _ _ _ ': _) =
    object
  GetNamedTypeR schema name (name ':-> 'INTERFACE _ _ ': _) =
    Var ('R (InterfaceToRowR name schema))
  GetNamedTypeR schema name (name ':-> UNION _ possibleTypes ': _) =
    Var (UnionToRowR schema possibleTypes)
  GetNamedTypeR schema name (name ':-> 'INPUT_OBJECT _ ('R inputFields) ': _) =
    Rec ('R (InputValuesToRowR schema inputFields))
  GetNamedTypeR schema name (_ ': rest) =
    GetNamedTypeR schema name rest

type family InterfaceToRowR (interface :: Symbol) (definitions :: [LT TypeDefinition]) :: [LT Type] where
  InterfaceToRowR _ '[] = '[]
  InterfaceToRowR interface (label :-> 'OBJECT object _ interfaces _ ': rest) =
    Ifte
      (Elem interface interfaces)
      (label :-> object ': InterfaceToRowR interface rest)
      (InterfaceToRowR interface rest)
  InterfaceToRowR interface (_ ': rest) = InterfaceToRowR interface rest

type family UnionToRowR (schema :: [LT TypeDefinition]) (possibleTypes :: [Symbol]) :: Row Type where
  UnionToRowR schema '[] = Empty
  UnionToRowR schema (label ': rest) =
    label .== GetNamedTypeR schema label schema
    .+ UnionToRowR schema rest

type family InputValuesToRowR (schema :: [LT TypeDefinition]) (inputValues :: [LT InputValueDefinition]) :: [LT Type] where
  InputValuesToRowR _ '[] = '[]
  InputValuesToRowR schema (label :-> 'INPUT_VALUE ref _ ': rest) =
    label :-> GetInputValueTypeR schema ref
    ': InputValuesToRowR schema rest
  InputValuesToRowR schema (label :-> 'INPUT_VALUE_WITH_DEFAULT ref _ _ ': rest) =
    label :-> GetNullableInputValueTypeR schema ref
    ': InputValuesToRowR schema rest

type family GetInputValueTypeR (schema :: [LT TypeDefinition]) (ref :: TypeRef) :: Type where
  GetInputValueTypeR schema ('NON_NULL symbol) = GetNamedTypeR schema symbol schema
  GetInputValueTypeR schema ('NON_NULL_LIST_OF ref) = [GetInputValueTypeR schema ref]
  GetInputValueTypeR schema ('NULLABLE symbol) = Maybe (GetNamedTypeR schema symbol schema)
  GetInputValueTypeR schema ('NULLABLE_LIST_OF ref) = Maybe [GetInputValueTypeR schema ref]

type family GetNullableInputValueTypeR (schema :: [LT TypeDefinition]) (ref :: TypeRef) :: Type where
  GetNullableInputValueTypeR schema ('NULLABLE symbol) = GetNamedTypeR schema symbol schema
  GetNullableInputValueTypeR schema ('NULLABLE_LIST_OF ref) = Maybe [GetInputValueTypeR schema ref]
  -- TODO: error message in case you try to provide a non-null input value with a default value


newtype GraphQLEnum (cases :: [Symbol]) = GraphQLEnum { value :: Text }

-- functions on type level lists

type family Elem (value :: k) (values :: [k]) :: Bool where
  Elem value '[] = False
  Elem value (value ': _) = True
  Elem value (_ ': rest) = Elem value rest
