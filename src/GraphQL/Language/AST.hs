module GraphQL.Language.AST where

import           Protolude

-- see: https://github.com/graphql/graphql-js/blob/master/src/language/ast.js
data AstNode node = AstNode
  { start :: Int
  , end   :: Int
  , node  :: node
  } deriving (Functor)

-- Name
newtype Name = Name
  { unName :: Text
  }

-- Document
newtype Document = Document
  { definitions :: [AstNode Definition]
  }

data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition
  | DefinitionSchema SchemaDefinition
  | DefinitionType TypeDefinition
  | DefinitionDirective DirectiveDefinition

data OperationType
  = Query
  | Mutation
  | Subscription

data OperationDefinition = OperationDefinition
  { operation           :: OperationType
  , name                :: Maybe (AstNode Name)
  , variableDefinitions :: [AstNode VariableDefinition]
  , directives          :: [AstNode Directive]
  , selectionSet        :: AstNode SelectionSet
  }

data VariableDefinition = VariableDefinition
  { variable     :: AstNode Variable
  , type_        :: AstNode Type_
  , defaultValue :: Maybe Value
  }

newtype Variable = Variable
  { name :: AstNode Name
  }

newtype SelectionSet = SelectionSet
  { selections :: [AstNode Selection]
  }

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment

data Field = Field
  { alias        :: Maybe (AstNode Name)
  , name         :: AstNode Name
  , arguments    :: [AstNode Argument]
  , directives   :: [AstNode Directive]
  , selectionSet :: AstNode SelectionSet
  }

data Argument = Argument
  { name  :: AstNode Name
  , value :: AstNode Value
  }

-- Fragments
data FragmentSpread = FragmentSpread
  { name       :: AstNode Name
  , directives :: [AstNode Directive]
  }

data InlineFragment = InlineFragment
  { typeCondition :: AstNode NamedType
  , directives    :: [AstNode Directive]
  , selectionSet  :: AstNode SelectionSet
  }

data FragmentDefinition = FragmentDefinition
  { name          :: AstNode Name
  , typeCondition :: AstNode NamedType
  , directives    :: [AstNode Directive]
  , selectionSet  :: AstNode SelectionSet
  }

-- Values
data Value
  = ValueVariable Variable
  | ValueInt Int32
  | ValueFloat Double
  | ValueString Text
  | ValueBoolean Bool
  | ValueNull
  | ValueEnum (AstNode Name)
  | ValueList (AstNode [AstNode Value])
  | ValueObject (AstNode [AstNode ObjectField])

newtype StringValue = StringValue
  { value :: Text
  }

data ObjectField = ObjectField
  { name  :: AstNode Name
  , value :: AstNode Value
  }

-- Directives
data Directive = Directive
  { name      :: AstNode Name
  , arguments :: [AstNode Argument]
  }

data DirectiveDefinition = DirectiveDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , arguments   :: [AstNode InputValueDefinition]
  , locations   :: [AstNode Name]
  }

-- Type reference
data Type_
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType

data NullableType
  = NullableNamed NamedType
  | NullableList ListType

newtype NamedType = NamedType
  { name :: AstNode Name
  }

newtype ListType = ListType
  { _type :: AstNode Type_
  }

newtype NonNullType = NonNullType
  { _type :: AstNode NullableType
  }

-- Type System
data SchemaDefinition = SchemaDefinition
  { directives     :: Directive
  , operationTypes :: [AstNode OperationTypeDefinition]
  }

data OperationTypeDefinition = OperationTypeDefinition
  { operation :: AstNode OperationType
  , _type     :: AstNode NamedType
  }

-- Type Definition
data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition

data ScalarTypeDefinition = ScalarDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , directives  :: [AstNode Directive]
  }

data ObjectTypeDefinition = ObjectTypeDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , interfaces  :: [AstNode NamedType]
  , directives  :: [AstNode Directive]
  , fields      :: [AstNode FieldDefinition]
  }

data InterfaceTypeDefinition = InterfaceTypeDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , directives  :: [AstNode Directive]
  , fields      :: [AstNode FieldDefinition]
  }

data UnionTypeDefinition = UnionTypeDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , directives  :: [AstNode Directive]
  , types       :: [AstNode NamedType]
  }

data EnumTypeDefinition = EnumTypeDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , directives  :: [AstNode Directive]
  , values      :: [AstNode EnumValueDefinition]
  }

data EnumValueDefinition = EnumValueDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , directives  :: [AstNode Directive]
  }

data InputObjectTypeDefinition = InputObjectTypeDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , directives  :: [AstNode Directive]
  , fields      :: [AstNode InputValueDefinition]
  }

data FieldDefinition = FieldDefinition
  { description :: Maybe (AstNode StringValue)
  , name        :: AstNode Name
  , arguments   :: [AstNode InputValueDefinition]
  , _type       :: AstNode Type_
  , directives  :: [AstNode Directive]
  }

data InputValueDefinition = InputValueDefinition
  { description  :: Maybe (AstNode StringValue)
  , name         :: AstNode Name
  , directives   :: [AstNode Directive]
  , defaultValue :: Maybe (AstNode Value)
  }
