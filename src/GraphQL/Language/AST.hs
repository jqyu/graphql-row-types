{-# LANGUAGE TypeApplications #-}

-- | This is copied from https://github.com/haskell-graphql/graphql-api/blob/master/src/GraphQL/Internal/Syntax/AST.hs
-- it's super messy and needs to be reviewed/cleaned up
-- the main thing that's missing right now is source positions
module GraphQL.Language.AST
  ( QueryDocument(..)
  , SchemaDocument(..)
  , Definition(..)
  , OperationDefinition(..)
  , Node(..)
  , VariableDefinition(..)
  , Variable(..)
  , SelectionSet
  , Selection(..)
  , Field(..)
  , Name(..)
  , Alias
  , Argument(..)
  , FragmentSpread(..)
  , InlineFragment(..)
  , FragmentDefinition(..)
  , TypeCondition
  , Value(..)
  , StringValue(..)
  , ListValue(..)
  , ObjectValue(..)
  , ObjectField(..)
  , DefaultValue
  , Directive(..)
  , GType(..)
  , NamedType(..)
  , ListType(..)
  , NonNullType(..)
  , TypeDefinition(..)
  , Description
  , ObjectTypeDefinition(..)
  , Interfaces
  , FieldDefinition(..)
  , ArgumentsDefinition
  , InputValueDefinition(..)
  , InterfaceTypeDefinition(..)
  , UnionTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , EnumTypeDefinition(..)
  , EnumValueDefinition(..)
  , InputObjectTypeDefinition(..)
  ) where

import           Protolude

import           Data.String     (String)
import           Test.QuickCheck (Arbitrary (..), arbitrary, elements, listOf,
                                  oneof)

-- * Name
-- | A name in GraphQL.
--
-- https://facebook.github.io/graphql/#sec-Names
newtype Name = Name
  { unName :: Text
  } deriving (Eq, Ord, Show)

instance Arbitrary Name where
  arbitrary = do
    initial <- elements alpha
    rest <- listOf (elements (alpha <> numeric))
    pure (Name (toS (initial : rest)))
    where
      alpha = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['_']
      numeric = ['0' .. '9']

-- * Documents
-- | A 'QueryDocument' is something a user might send us.
--
-- https://facebook.github.io/graphql/#sec-Language.Query-Document
newtype QueryDocument = QueryDocument
  { getDefinitions :: [Definition]
  } deriving (Eq, Show)

data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition
  deriving (Eq, Show)

data OperationDefinition
  = Query Node
  | Mutation Node
  | AnonymousQuery SelectionSet
  deriving (Eq, Show)

data Node =
  Node (Maybe Name)
       [VariableDefinition]
       [Directive]
       SelectionSet
  deriving (Eq, Show)

data VariableDefinition =
  VariableDefinition Variable
                     GType
                     (Maybe DefaultValue)
  deriving (Eq, Show)

newtype Variable =
  Variable Name
  deriving (Eq, Ord, Show)

instance Arbitrary Variable where
  arbitrary = Variable <$> arbitrary

type SelectionSet = [Selection]

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment
  deriving (Eq, Show)

data Field =
  Field (Maybe Alias)
        Name
        [Argument]
        [Directive]
        SelectionSet
  deriving (Eq, Show)

type Alias = Name

data Argument =
  Argument Name
           Value
  deriving (Eq, Show)

-- * Fragments
data FragmentSpread =
  FragmentSpread Name
                 [Directive]
  deriving (Eq, Show)

data InlineFragment =
  InlineFragment (Maybe TypeCondition)
                 [Directive]
                 SelectionSet
  deriving (Eq, Show)

data FragmentDefinition =
  FragmentDefinition Name
                     TypeCondition
                     [Directive]
                     SelectionSet
  deriving (Eq, Show)

type TypeCondition = NamedType

-- * Values
data Value
  = ValueVariable Variable
  | ValueInt Int32
  | ValueFloat Double
  | ValueBoolean Bool
  | ValueString StringValue
  | ValueEnum Name
  | ValueList ListValue
  | ValueObject ObjectValue
  | ValueNull
  deriving (Eq, Ord, Show)

instance Arbitrary Value where
  arbitrary =
    oneof
      [ ValueVariable <$> arbitrary
      , ValueInt <$> arbitrary
      , ValueFloat <$> arbitrary
      , ValueBoolean <$> arbitrary
      , ValueString <$> arbitrary
      , ValueEnum <$> arbitrary
      , ValueList <$> arbitrary
      , ValueObject <$> arbitrary
      , pure ValueNull
      ]

newtype StringValue =
  StringValue Text
  deriving (Eq, Ord, Show)

instance Arbitrary StringValue where
  arbitrary = StringValue . toS <$> arbitrary @String

newtype ListValue =
  ListValue [Value]
  deriving (Eq, Ord, Show)

instance Arbitrary ListValue where
  arbitrary = ListValue <$> listOf arbitrary

newtype ObjectValue =
  ObjectValue [ObjectField]
  deriving (Eq, Ord, Show)

instance Arbitrary ObjectValue where
  arbitrary = ObjectValue <$> listOf arbitrary

data ObjectField =
  ObjectField Name
              Value
  deriving (Eq, Ord, Show)

instance Arbitrary ObjectField where
  arbitrary = ObjectField <$> arbitrary <*> arbitrary

type DefaultValue = Value

-- * Directives
data Directive =
  Directive Name
            [Argument]
  deriving (Eq, Show)

-- * Type Reference
data GType
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType
  deriving (Eq, Ord, Show)

newtype NamedType =
  NamedType Name
  deriving (Eq, Ord, Show)

newtype ListType =
  ListType GType
  deriving (Eq, Ord, Show)

data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList ListType
  deriving (Eq, Ord, Show)

-- | A 'SchemaDocument' is a document that defines a GraphQL schema.
--
-- https://facebook.github.io/graphql/#sec-Type-System
newtype SchemaDocument =
  SchemaDocument [TypeDefinition]
  deriving (Eq, Show)

-- * Type definition
data TypeDefinition
  = TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Eq, Show)

-- TODO: support type extensions
data ObjectTypeDefinition =
  ObjectTypeDefinition Description
                       Name
                       Interfaces
                       [Directive]
                       [FieldDefinition]
  deriving (Eq, Show)

type Interfaces = [NamedType]

type Description = Maybe StringValue

data FieldDefinition =
  FieldDefinition Description
                  Name
                  ArgumentsDefinition
                  GType
                  [Directive]
  deriving (Eq, Show)

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition =
  InputValueDefinition Description
                       Name
                       GType
                       (Maybe DefaultValue)
                       [Directive]
  deriving (Eq, Show)

data InterfaceTypeDefinition =
  InterfaceTypeDefinition Description
                          Name
                          [Directive]
                          [FieldDefinition]
  deriving (Eq, Show)

data UnionTypeDefinition =
  UnionTypeDefinition Description
                      Name
                      [Directive]
                      [NamedType]
  deriving (Eq, Show)

data ScalarTypeDefinition =
  ScalarTypeDefinition Description
                       Name
                       [Directive]
  deriving (Eq, Show)

data EnumTypeDefinition =
  EnumTypeDefinition Description
                     Name
                     [Directive]
                     [EnumValueDefinition]
  deriving (Eq, Show)

data EnumValueDefinition =
  EnumValueDefinition Description
                      Name
                      [Directive]
  deriving (Eq, Show)

data InputObjectTypeDefinition =
  InputObjectTypeDefinition Description
                            Name
                            [Directive]
                            [InputValueDefinition]
  deriving (Eq, Show)
