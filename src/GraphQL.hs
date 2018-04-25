module GraphQL
  ( parseQuery
  , parseSchema
  , printQuery
  , printSchema
  ) where

import           Protolude

import qualified Data.Attoparsec.Text     as Attoparsec
import           Data.String              (String)
import qualified GraphQL.Language.AST     as AST
import qualified GraphQL.Language.Encoder as Encoder
import qualified GraphQL.Language.Parser  as Parser


parseQuery :: Text -> Either String AST.QueryDocument
parseQuery = Attoparsec.parseOnly (Parser.queryDocument <* Attoparsec.endOfInput)


parseSchema :: Text -> Either String AST.SchemaDocument
parseSchema = Attoparsec.parseOnly (Parser.schemaDocument <* Attoparsec.endOfInput)


printQuery :: AST.QueryDocument -> Text
printQuery = Encoder.queryDocument


printSchema :: AST.SchemaDocument -> Text
printSchema = Encoder.schemaDocument
