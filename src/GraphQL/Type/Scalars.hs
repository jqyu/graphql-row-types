-- this module provides the resolvers for the built-in GraphQL scalars
{-# LANGUAGE QuasiQuotes #-}

module GraphQL.Type.Scalars
  ( Id(..)
  , Definitions
  , resolver
  ) where

import Protolude

import GraphQL.Language.AST as AST
import GraphQL.Resolver (define, combine, ScalarResolver(..))
import qualified GraphQL.Resolver as GraphQL (Resolver)
import qualified GraphQL.Type.QuasiQuoter as GraphQL (schema)


newtype Id = Id { unId :: Text } deriving (Eq, Ord, Show)


type Definitions = [GraphQL.schema|
  """
  The `Int` scalar type represents non-fractional signed whole numeric
  values. Int can represent values between -(2^31) and 2^31 - 1.
  """
  scalar Int @haskellType(name: "Int32")


  """
  The `Float` scalar type represents signed double-precision fractional
  values as specified by
  [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point).
  """
  scalar Float @haskellType(name: "Double")

  """
  The `String` scalar type represents textual data, represented as UTF-8
  character sequences. The String type is most often used by GraphQL to
  represent free-form human-readable text.
  """
  scalar String @haskellType(name: "Text")

  """
  The `Boolean` scalar type represents `true` or `false`.
  """
  scalar Boolean @haskellType(name: "Bool")

  """
  The `ID` scalar type represents a unique identifier, often used to
  refetch an object or as key for a cache. The ID type appears in a JSON
  response as a String; however, it is not intended to be human-readable.
  When expected as an input type, any string (such as `"4"`) or integer
  (such as `4`) input value will be accepted as an ID.
  """
  scalar ID @haskellType(name: "Id")
|]

resolver :: forall m schema. GraphQL.Resolver m schema Definitions
resolver =
  define @"Int" ScalarResolver
    { serialize = const AST.ValueNull
    , deserialize = const Nothing
    }
  `combine`
  define @"Float" ScalarResolver
    { serialize = const AST.ValueNull
    , deserialize = const Nothing
    }
  `combine`
  define @"String" ScalarResolver
    { serialize = const AST.ValueNull
    , deserialize = const Nothing
    }
  `combine`
  define @"Boolean" ScalarResolver
    { serialize = const AST.ValueNull
    , deserialize = const Nothing
    }
  `combine`
  define @"ID" ScalarResolver
    { serialize = const AST.ValueNull
    , deserialize = const Nothing
    }
