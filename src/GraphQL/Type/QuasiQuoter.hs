module GraphQL.Type.QuasiQuoter
  ( schema
  ) where

import Control.Monad (fail)
import qualified Data.Attoparsec.Text as AParsec
       (endOfInput, parseOnly)
import qualified Data.Map as Map
import Data.Row.Poly (Row(..), LT(..))
import qualified GraphQL.Type as GraphQL
import qualified GraphQL.Language.AST as AST
import qualified GraphQL.Language.Encoder as Encoder
import qualified GraphQL.Language.Parser as Parser (schemaDocument)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Q, TyLit(..), Type(..), lookupTypeName)
import Protolude hiding (Type)

schema :: QuasiQuoter
schema =
  QuasiQuoter
  { quoteExp = const $ fail $ errorString "an expression"
  , quotePat = const $ fail $ errorString "a pattern"
  , quoteType = sdlToType . toS
  , quoteDec = const $ fail $ errorString "a declaration"
  }
  where
    errorString position =
      "You have used the `GraphQL.schema` QuasiQuoter in " <> position <>
      "context;" <>
      "you must only use it in a type context, i.e. type MyDefinitions = [GraphQL.schema|...|]"

sdlToType :: Text -> Q Type
sdlToType source = do
  AST.SchemaDocument typeDefinitions <-
    either (fail . ("parse error: " <>)) pure
    . AParsec.parseOnly (Parser.schemaDocument <* AParsec.endOfInput)
    $ source
  rows <- for typeDefinitions $ \case
    AST.TypeDefinitionScalar def ->
      scalarT def
    AST.TypeDefinitionObject def ->
      -- TODO: check field uniqueness
      -- TODO: check that interfaces are either interfaces or not defined
      -- TODO: check that interfaces don't have duplicates
      objectT def
    AST.TypeDefinitionInterface def ->
      interfaceT def
    AST.TypeDefinitionUnion def ->
      -- TODO: check that union members don't have duplicates
      -- TODO: check that union members are either objects or not defined
      pure (unionT def)
    AST.TypeDefinitionEnum def ->
      enumT def
    AST.TypeDefinitionInputObject def ->
      inputObjectT def
  rowsT rows


symbolT :: Text -> Type
symbolT = LitT . StrTyLit . toS

nameT :: AST.Name -> Type
nameT = symbolT . AST.unName

descriptionT :: AST.Description -> Type
descriptionT =
  maybe (LitT (StrTyLit "")) $ \(AST.StringValue value) ->
    symbolT value

type RowT = (AST.Name, Type)

rowT :: AST.Name -> Type -> RowT
rowT name t = (name, t)

-- constructs a row type, taking care to preserve label ordering
-- the alternative would be to construct the type via applications of
-- '.==' and '.+', which lead to slower type-checking
rowsT :: [RowT] -> Q Type
rowsT = map mapToRow . foldM collectEntry Map.empty
  where
    mapToRow :: Map AST.Name Type -> Type
    mapToRow = AppT (PromotedT 'R) . promotedListT . map toLT . Map.toAscList

    toLT :: RowT -> Type
    toLT (name, t) =
      PromotedT '(:->)
      `AppT` nameT name
      `AppT` t

    collectEntry :: Map AST.Name Type -> RowT -> Q (Map AST.Name Type)
    collectEntry entries (name, _) | Map.member name entries =
      fail "Duplicate row entry. TODO: provide better error message"
    collectEntry entries (name, t) = pure (Map.insert name t entries)

promotedListT :: [Type] -> Type
promotedListT = foldr (AppT . AppT PromotedConsT) PromotedNilT

scalarT :: AST.ScalarTypeDefinition -> Q RowT
scalarT (AST.ScalarTypeDefinition desc name ds) = do
  haskellType <- haskellTypeDirective (ScalarDef name) ds
  pure . rowT name $
    PromotedT 'GraphQL.SCALAR
    `AppT` haskellType
    `AppT` descriptionT desc

objectT :: AST.ObjectTypeDefinition -> Q RowT
objectT (AST.ObjectTypeDefinition desc name ifs ds fs) = do
  haskellType <- haskellTypeDirective (ObjectDef name) ds
  fields <- traverse fieldT fs
  fieldsRow <- rowsT fields
  pure . rowT name $
    PromotedT 'GraphQL.OBJECT
    `AppT` haskellType
    `AppT` descriptionT desc
    `AppT` promotedListT (map (\(AST.NamedType n) -> nameT n) ifs)
    `AppT` fieldsRow

interfaceT :: AST.InterfaceTypeDefinition -> Q RowT
interfaceT (AST.InterfaceTypeDefinition desc name _ fs) = do
  fields <- traverse fieldT fs
  fieldsRow <- rowsT fields
  pure . rowT name $
    PromotedT 'GraphQL.INTERFACE
    `AppT` descriptionT desc
    `AppT` fieldsRow

unionT :: AST.UnionTypeDefinition -> RowT
unionT (AST.UnionTypeDefinition desc name _ ts) =
  rowT name
  $ PromotedT 'GraphQL.UNION
    `AppT` descriptionT desc
    `AppT` promotedListT (map (\(AST.NamedType n) -> nameT n) ts)

enumT :: AST.EnumTypeDefinition -> Q RowT
enumT (AST.EnumTypeDefinition desc name _ vals) = do
  valsRow <- rowsT $ map enumValueT vals
  pure . rowT name $
    PromotedT 'GraphQL.ENUM
    `AppT` descriptionT desc
    `AppT` valsRow

inputObjectT :: AST.InputObjectTypeDefinition -> Q RowT
inputObjectT (AST.InputObjectTypeDefinition desc name _ fs) = do
  inputValuesRow <- rowsT $ map inputValueT fs
  pure . rowT name $
    PromotedT 'GraphQL.INPUT_OBJECT
    `AppT` descriptionT desc
    `AppT` inputValuesRow


typeRefT :: AST.GType -> Type
typeRefT (AST.TypeNamed (AST.NamedType n)) =
  PromotedT 'GraphQL.NULLABLE `AppT` nameT n
typeRefT (AST.TypeList (AST.ListType t)) =
  PromotedT 'GraphQL.NULLABLE_LIST_OF `AppT` typeRefT t
typeRefT (AST.TypeNonNull (AST.NonNullTypeNamed (AST.NamedType n))) =
  PromotedT 'GraphQL.NON_NULL `AppT` nameT n
typeRefT (AST.TypeNonNull (AST.NonNullTypeList (AST.ListType t))) =
  PromotedT 'GraphQL.NON_NULL_LIST_OF `AppT` typeRefT t

fieldT :: AST.FieldDefinition -> Q RowT
fieldT (AST.FieldDefinition desc name args t ds) = do
  argsRow <- rowsT $ map inputValueT args
  pure . rowT name $ case deprecationDirective ds of
    Just reason ->
      PromotedT 'GraphQL.DEPRECATED_FIELD
      `AppT` argsRow
      `AppT` typeRefT t
      `AppT` descriptionT reason
      `AppT` descriptionT desc
    Nothing ->
      PromotedT 'GraphQL.FIELD
      `AppT` argsRow
      `AppT` typeRefT t
      `AppT` descriptionT desc

inputValueT :: AST.InputValueDefinition -> RowT
inputValueT (AST.InputValueDefinition desc name t d _) =
  rowT name $ case d of
    Just v ->
      PromotedT 'GraphQL.INPUT_VALUE_WITH_DEFAULT
      `AppT` typeRefT t
      `AppT` symbolT (Encoder.value v)
      `AppT` descriptionT desc
    Nothing ->
      PromotedT 'GraphQL.INPUT_VALUE
      `AppT` typeRefT t
      `AppT` descriptionT desc

enumValueT :: AST.EnumValueDefinition -> RowT
enumValueT (AST.EnumValueDefinition desc name ds) =
  rowT name $ case deprecationDirective ds of
    Just reason ->
      PromotedT 'GraphQL.DEPRECATED_ENUM_VALUE
      `AppT` descriptionT reason
      `AppT` descriptionT desc
    Nothing ->
      PromotedT 'GraphQL.ENUM_VALUE
      `AppT` descriptionT desc

-- here be dragons
getDirective :: Text -> [AST.Directive] -> Maybe AST.Directive
getDirective expected = listToMaybe . mapMaybe f
  where
    f d@(AST.Directive (AST.Name name) _)
      | name == expected = Just d
      | otherwise = Nothing


deprecationDirective :: [AST.Directive] -> Maybe AST.Description
deprecationDirective = map getReason . getDirective "deprecated"
  where
    getReason :: AST.Directive -> AST.Description
    getReason (AST.Directive _ [AST.Argument (AST.Name "reason") (AST.ValueString v)]) = Just v
    getReason _ = Nothing


data HaskellTypeDirectiveContext
  = ObjectDef AST.Name
  | ScalarDef AST.Name

haskellTypeDirective :: HaskellTypeDirectiveContext -> [AST.Directive] -> Q Type
haskellTypeDirective ctx = catchMissing . map toType . getDirective "haskellType"
  where
    defContext = case ctx of
      ObjectDef name ->
        "`type " <> toS (AST.unName name) <> "`"
      ScalarDef name ->
        "`scalar " <> toS (AST.unName name) <> "`"

    catchMissing :: Maybe (Q Type) -> Q Type
    catchMissing (Just t) = t
    catchMissing Nothing =
      fail $ case ctx of
        ObjectDef name ->
          "No haskellType directive found for " <> defContext <> ". "
          <> "It is required to provide a type annotation such as "
          <> "`type " <> toS (AST.unName name) <> " implements I1 & I2 @haskellType(name: \"MyType\")`"
        ScalarDef name ->
          "No haskellType directive found for " <> defContext <> ". "
          <> "It is required to provide a type annotation such as "
          <> "`scalar " <> toS (AST.unName name) <> " @haskellType(name: \"MyType\")`"

    toType :: AST.Directive -> Q Type
    toType d@(AST.Directive _ arguments) = do
      nameArg <- case arguments of
        [AST.Argument (AST.Name "name") (AST.ValueString (AST.StringValue v))] ->
          pure (toS v)
        _ ->
          fail
          $ "Invalid directive: " <> toS (Encoder.directive d) <> " for " <> defContext <> ";"
            <> "Expected haskellType(name: String!)"
      typeName <- lookupTypeName nameArg
      case typeName of
        Nothing ->
          fail
          $ "No type " <> nameArg <> " in scope for " <> defContext
        Just n ->
          pure (ConT n)
