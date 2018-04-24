-- this module provides the resolvers for the built-in GraphQL scalars

module GraphQL.Type.Scalars
  ( Id(..)
  , Definitions
  , resolver
  ) where

import Protolude

import Data.Row.Poly (type (.==), type (.+))
import GraphQL.Language.AST as AST
import GraphQL.Resolver (define, combine, ScalarResolver(..))
import qualified GraphQL.Resolver as GraphQL (Resolver)
import GraphQL.Type


newtype Id = Id { unId :: Text } deriving (Eq, Ord, Show)


type Definitions =
  IntDefinition
  .+ FloatDefinition
  .+ StringDefinition
  .+ BooleanDefinition
  .+ IdDefinition

resolver :: forall m schema. GraphQL.Resolver m schema Definitions
resolver =
  int `combine` float `combine` string `combine` boolean `combine` id


type IntDefinition =
  "Int" .==
    'SCALAR Int32
      "The `Int` scalar type represents non-fractional signed whole numeric \
      \values. Int can represent values between -(2^31) and 2^31 - 1."

int :: forall m schema. GraphQL.Resolver m schema IntDefinition
int = define ScalarResolver
  { serialize = const AST.ValueNull
  , deserialize = const Nothing
  }


type FloatDefinition =
  "Float" .==
    'SCALAR Double
      "The `Float` scalar type represents signed double-precision fractional \
      \values as specified by \
      \[IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."

float :: forall m schema. GraphQL.Resolver m schema FloatDefinition
float = define ScalarResolver
  { serialize = const AST.ValueNull
  , deserialize = const Nothing
  }


type StringDefinition =
  "String" .==
    'SCALAR Text
      "The `String` scalar type represents textual data, represented as UTF-8 \
      \character sequences. The String type is most often used by GraphQL to \
      \represent free-form human-readable text."

string :: forall m schema. GraphQL.Resolver m schema StringDefinition
string = define ScalarResolver
  { serialize = const AST.ValueNull
  , deserialize = const Nothing
  }


type BooleanDefinition =
  "Boolean" .==
    'SCALAR Bool
      "The `Boolean` scalar type represents `true` or `false`."

boolean :: forall m schema. GraphQL.Resolver m schema BooleanDefinition
boolean = define ScalarResolver
  { serialize = const AST.ValueNull
  , deserialize = const Nothing
  }


type IdDefinition =
  "ID" .==
    'SCALAR Id
      "The `ID` scalar type represents a unique identifier, often used to \
      \refetch an object or as key for a cache. The ID type appears in a JSON \
      \response as a String; however, it is not intended to be human-readable. \
      \When expected as an input type, any string (such as `\"4\"`) or integer \
      \(such as `4`) input value will be accepted as an ID."

id :: forall m schema. GraphQL.Resolver m schema IdDefinition
id = define ScalarResolver
  { serialize = const AST.ValueNull
  , deserialize = const Nothing
  }
