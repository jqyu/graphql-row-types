-- | This is copied from https://github.com/haskell-graphql/graphql-api/blob/master/src/GraphQL/Internal/Syntax/Parser.hs
-- it's super messy and needs to be reviewed/cleaned up
module GraphQL.Language.Parser
  ( nameParser
  , queryDocument
  , schemaDocument
  , value
  , description
  ) where

import           Protolude                  hiding (takeWhile)

import           Control.Applicative        (empty, many, optional, (<|>))
import           Control.Monad              (fail)
import           Data.Aeson.Parser          (jstring)
import qualified Data.Attoparsec.ByteString as A
import           Data.Attoparsec.Text       (Parser, anyChar, char, endOfLine,
                                             inClass, many1, manyTill, match,
                                             option, peekChar, scan, scientific,
                                             sepBy1, takeWhile, takeWhile1,
                                             (<?>))
import           Data.Char                  (isDigit, isSpace)
import qualified Data.List                  as List (dropWhile, dropWhileEnd,
                                                     splitAt)
import           Data.Scientific            (floatingOrInteger)
import qualified Data.Text                  as Text (drop, dropEnd, find,
                                                     intercalate, length, lines,
                                                     takeEnd, takeWhile)

import qualified GraphQL.Language.AST       as AST

tok :: Parser a -> Parser a
tok p = p <* whiteSpace

whiteSpace :: Parser ()
whiteSpace =
  peekChar >>=
  traverse_
    (\c ->
       if isSpace c || c == ','
         then anyChar *> whiteSpace
         else when (c == '#') $ manyTill anyChar endOfLine *> whiteSpace)

-- * Name
nameParser :: Parser AST.Name
nameParser =
  AST.Name <$>
  tok ((<>) <$> takeWhile1 isA_z <*> takeWhile ((||) <$> isDigit <*> isA_z))
    -- `isAlpha` handles many more Unicode Chars
  where
    isA_z = inClass $ '_' : ['A' .. 'Z'] <> ['a' .. 'z']

-- * Document
queryDocument :: Parser AST.QueryDocument
queryDocument =
  whiteSpace *>
  (AST.QueryDocument <$> many1 definition) <?> "query document error!"

-- | Parser for a schema document.
schemaDocument :: Parser AST.SchemaDocument
schemaDocument =
  whiteSpace *>
  (AST.SchemaDocument <$> many1 typeDefinition) <?> "type document error"

definition :: Parser AST.Definition
definition =
  AST.DefinitionOperation <$> operationDefinition <|>
  AST.DefinitionFragment <$> fragmentDefinition <?> "definition error!"

operationDefinition :: Parser AST.OperationDefinition
operationDefinition =
  AST.Query <$ tok "query" <*> node <|> AST.Mutation <$ tok "mutation" <*> node <|>
  (AST.AnonymousQuery <$> selectionSet) <?> "operationDefinition error!"

node :: Parser AST.Node
node =
  AST.Node <$> optional nameParser <*> optempty variableDefinitions <*>
  optempty directives <*>
  selectionSet

variableDefinitions :: Parser [AST.VariableDefinition]
variableDefinitions = parens (many1 variableDefinition)

variableDefinition :: Parser AST.VariableDefinition
variableDefinition =
  AST.VariableDefinition <$> variable <* tok ":" <*> type_ <*>
  optional defaultValue

defaultValue :: Parser AST.DefaultValue
defaultValue = tok "=" *> value

variable :: Parser AST.Variable
variable = AST.Variable <$ tok "$" <*> nameParser

selectionSet :: Parser AST.SelectionSet
selectionSet = braces $ many1 selection

selection :: Parser AST.Selection
selection =
  AST.SelectionField <$> field <|>
  AST.SelectionInlineFragment <$> inlineFragment <|>
  AST.SelectionFragmentSpread <$> fragmentSpread <?> "selection error!"

field :: Parser AST.Field
field =
  AST.Field <$> option empty (pure <$> alias) <*> nameParser <*>
  optempty arguments <*>
  optempty directives <*>
  optempty selectionSet

alias :: Parser AST.Alias
alias = nameParser <* tok ":"

arguments :: Parser [AST.Argument]
arguments = parens $ many1 argument

argument :: Parser AST.Argument
argument = AST.Argument <$> nameParser <* tok ":" <*> value

-- * Fragments
fragmentSpread :: Parser AST.FragmentSpread
-- TODO: Make sure it fails when `... on`.
-- See https://facebook.github.io/graphql/#FragmentSpread
fragmentSpread =
  AST.FragmentSpread <$ tok "..." <*> nameParser <*> optempty directives

-- InlineFragment tried first in order to guard against 'on' keyword
inlineFragment :: Parser AST.InlineFragment
inlineFragment =
  AST.InlineFragment <$ tok "..." <*> optional (tok "on" *> typeCondition) <*>
  optempty directives <*>
  selectionSet

fragmentDefinition :: Parser AST.FragmentDefinition
fragmentDefinition =
  AST.FragmentDefinition <$ tok "fragment" <*> nameParser <* tok "on" <*>
  typeCondition <*>
  optempty directives <*>
  selectionSet

typeCondition :: Parser AST.TypeCondition
typeCondition = namedType

-- * Values
-- This will try to pick the first type it can parse. If you are working with
-- explicit types use the `typedValue` parser.
value :: Parser AST.Value
value =
  tok
    (AST.ValueVariable <$> (variable <?> "variable") <|> (number <?> "number") <|>
     AST.ValueNull <$ tok "null" <|>
     AST.ValueBoolean <$> (booleanValue <?> "booleanValue") <|>
     AST.ValueString <$> (stringValue <?> "stringValue") <|>
     AST.ValueEnum <$> (nameParser <?> "name") <|>
     AST.ValueList <$> (listValue <?> "listValue") <|>
     AST.ValueObject <$> (objectValue <?> "objectValue") <?> "value error!")
  where
    number = do
      (numText, num) <- match (tok scientific)
      case (Text.find (== '.') numText, floatingOrInteger num) of
        (Just _, Left r)   -> pure (AST.ValueFloat r)
        (Just _, Right i)  -> pure (AST.ValueFloat (fromIntegral i))
        -- TODO: Handle maxBound, Int32 in spec.
        (Nothing, Left r)  -> pure (AST.ValueInt (floor r))
        (Nothing, Right i) -> pure (AST.ValueInt i)

booleanValue :: Parser Bool
booleanValue = True <$ tok "true" <|> False <$ tok "false"

stringValue :: Parser AST.StringValue
stringValue = blockStringValue <|> nonBlockStringValue

nonBlockStringValue :: Parser AST.StringValue
nonBlockStringValue = do
  parsed <- char '"' *> jstring_
  case unescapeText parsed of
    Left err      -> fail err
    Right escaped -> pure (AST.StringValue escaped)
    -- | Parse a string without a leading quote, ignoring any escaped characters.
  where
    jstring_ :: Parser Text
    jstring_ = scan startState go <* anyChar
    startState = False
    go a c
      | a = Just False
      | c == '"' = Nothing
      | otherwise =
        let a' = c == backslash
        in Just a'
      where
        backslash = '\\'
    -- | Unescape a string.
    --
    -- Turns out this is really tricky, so we're going to cheat by
    -- reconstructing a literal string (by putting quotes around it) and
    -- delegating all the hard work to Aeson.
    unescapeText str = A.parseOnly jstring ("\"" <> toS str <> "\"")

blockStringValue :: Parser AST.StringValue
blockStringValue = do
  rawValue <- "\"\"\"" *> blockString
  case Text.takeEnd 3 rawValue of
    "\"\"\"" -> pure . AST.StringValue . normalize . Text.dropEnd 3 $ rawValue
    _ -> fail ("Unterminated block string:\n\"\"\"" <> toS rawValue)
  where
    blockString :: Parser Text
    blockString = scan startState go
    -- (whether or not we have seen a backslash, number of consecutive quotes)
    startState :: (Bool, Int)
    startState = (False, 0)
    go :: (Bool, Int) -> Char -> Maybe (Bool, Int)
    go _ '\\' = Just (True, 0)
    go (True, n) '"'
      | n >= 3 = Just (False, 1)
    go (True, n) '"' = Just (True, n + 1)
    go (True, _) _ = Just (False, 0)
    go (False, n) _
      | n >= 3 = Nothing
    go (False, n) '"' = Just (False, n + 1)
    go (False, _) _ = Just (False, 0)
    -- see: http://facebook.github.io/graphql/draft/#BlockStringValue()
    normalize :: Text -> Text
    normalize rawValue =
      Text.intercalate "\n" .
      map snd .
      List.dropWhile ((Nothing ==) . fst) .
      List.dropWhileEnd ((Nothing ==) . fst) $
      firstLine ++ map dropCommonIndent rest
      where
        (firstLine, rest) = List.splitAt 1 . map getIndent $ Text.lines rawValue
        getIndent :: Text -> (Maybe Int, Text)
        getIndent line =
          case Text.length (Text.takeWhile isSpace line) of
            indent
              | indent < Text.length line -> (Just indent, line)
              | otherwise -> (Nothing, line)
        minIndent :: Maybe Int -> Maybe Int -> Maybe Int
        minIndent Nothing v           = v
        minIndent v Nothing           = v
        minIndent (Just v1) (Just v2) = Just (min v1 v2)
        commonIndent :: Int
        commonIndent = fromMaybe 0 $ foldr (minIndent . fst) Nothing rest
        dropCommonIndent :: (Maybe Int, Text) -> (Maybe Int, Text)
        dropCommonIndent (indent, line) = (indent, Text.drop commonIndent line)

-- Notice it can be empty
listValue :: Parser AST.ListValue
listValue = AST.ListValue <$> brackets (many value)

-- Notice it can be empty
objectValue :: Parser AST.ObjectValue
objectValue = AST.ObjectValue <$> braces (many (objectField <?> "objectField"))

objectField :: Parser AST.ObjectField
objectField = AST.ObjectField <$> nameParser <* tok ":" <*> value

-- * Directives
directives :: Parser [AST.Directive]
directives = many1 directive

directive :: Parser AST.Directive
directive = AST.Directive <$ tok "@" <*> nameParser <*> optempty arguments

-- * Type Reference
type_ :: Parser AST.GType
type_ =
  AST.TypeNonNull <$> nonNullType <|>
  AST.TypeList <$> listType <|>
  AST.TypeNamed <$> namedType <?> "type_ error!"

namedType :: Parser AST.NamedType
namedType = AST.NamedType <$> nameParser

listType :: Parser AST.ListType
listType = AST.ListType <$> brackets type_

nonNullType :: Parser AST.NonNullType
nonNullType =
  AST.NonNullTypeNamed <$> namedType <* tok "!" <|>
  AST.NonNullTypeList <$> listType <* tok "!" <?> "nonNullType error!"

-- * Type Definition
typeDefinition :: Parser AST.TypeDefinition
typeDefinition =
  AST.TypeDefinitionObject <$> objectTypeDefinition <|>
  AST.TypeDefinitionInterface <$> interfaceTypeDefinition <|>
  AST.TypeDefinitionUnion <$> unionTypeDefinition <|>
  AST.TypeDefinitionScalar <$> scalarTypeDefinition <|>
  AST.TypeDefinitionEnum <$> enumTypeDefinition <|>
  AST.TypeDefinitionInputObject <$>
  inputObjectTypeDefinition <?> "typeDefinition error!"

description :: Parser AST.Description
description = optional (tok stringValue)

objectTypeDefinition :: Parser AST.ObjectTypeDefinition
objectTypeDefinition =
  AST.ObjectTypeDefinition <$> description <* tok "type" <*> nameParser <*>
  optempty interfaces <*>
  optempty directives <*>
  fieldDefinitions

interfaces :: Parser AST.Interfaces
interfaces =
  tok "implements" *> optional (tok "&") *> (namedType `sepBy1` tok "&")

fieldDefinitions :: Parser [AST.FieldDefinition]
fieldDefinitions = braces $ many1 fieldDefinition

fieldDefinition :: Parser AST.FieldDefinition
fieldDefinition =
  AST.FieldDefinition <$> description <*> nameParser <*>
  optempty argumentsDefinition <*
  tok ":" <*>
  type_ <*>
  optempty directives

argumentsDefinition :: Parser AST.ArgumentsDefinition
argumentsDefinition = parens $ many1 inputValueDefinition

interfaceTypeDefinition :: Parser AST.InterfaceTypeDefinition
interfaceTypeDefinition =
  AST.InterfaceTypeDefinition <$> description <* tok "interface" <*> nameParser <*>
  optempty directives <*>
  fieldDefinitions

unionTypeDefinition :: Parser AST.UnionTypeDefinition
unionTypeDefinition =
  AST.UnionTypeDefinition <$> description <* tok "union" <*> nameParser <*>
  optempty directives <*
  tok "=" <*
  optional (tok "|") <*>
  unionMembers

unionMembers :: Parser [AST.NamedType]
unionMembers = namedType `sepBy1` tok "|"

scalarTypeDefinition :: Parser AST.ScalarTypeDefinition
scalarTypeDefinition =
  AST.ScalarTypeDefinition <$> description <* tok "scalar" <*> nameParser <*>
  optempty directives

enumTypeDefinition :: Parser AST.EnumTypeDefinition
enumTypeDefinition =
  AST.EnumTypeDefinition <$> description <* tok "enum" <*> nameParser <*>
  optempty directives <*>
  enumValueDefinitions

enumValueDefinitions :: Parser [AST.EnumValueDefinition]
enumValueDefinitions = braces $ many1 enumValueDefinition

enumValueDefinition :: Parser AST.EnumValueDefinition
enumValueDefinition =
  AST.EnumValueDefinition <$> description <*> nameParser <*> optempty directives

inputObjectTypeDefinition :: Parser AST.InputObjectTypeDefinition
inputObjectTypeDefinition =
  AST.InputObjectTypeDefinition <$> description <* tok "input" <*> nameParser <*>
  optempty directives <*>
  inputValueDefinitions

inputValueDefinitions :: Parser [AST.InputValueDefinition]
inputValueDefinitions = braces $ many1 inputValueDefinition

inputValueDefinition :: Parser AST.InputValueDefinition
inputValueDefinition =
  AST.InputValueDefinition <$> description <*> nameParser <* tok ":" <*> type_ <*>
  optional defaultValue <*>
  optempty directives

-- * Internal
parens :: Parser a -> Parser a
parens = between "(" ")"

braces :: Parser a -> Parser a
braces = between "{" "}"

brackets :: Parser a -> Parser a
brackets = between "[" "]"

between :: Parser Text -> Parser Text -> Parser a -> Parser a
between open close p = tok open *> p <* tok close

-- `empty` /= `pure mempty` for `Parser`.
optempty :: Monoid a => Parser a -> Parser a
optempty = option mempty
