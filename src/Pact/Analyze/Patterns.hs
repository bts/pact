{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE ViewPatterns      #-}

module Pact.Analyze.Patterns where

import           Data.Monoid          ((<>))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)

import qualified Pact.Types.Lang      as Lang
import           Pact.Types.Runtime   (BindType (BindLet, BindSchema),
                                       Literal (LString))
import           Pact.Types.Typecheck (AST (App, Binding, List, Object, Prim, Table),
                                       Fun (FDefun, FNative), Named, Node,
                                       PrimValue (PrimLit), Special (SBinding))
import qualified Pact.Types.Typecheck as TC

comparisonOperators, logicalOperators, arithOperators :: Set Text
comparisonOperators = Set.fromList [">", "<", ">=", "<=", "=", "!="]
logicalOperators    = Set.fromList ["and", "or", "not"]
arithOperators      = Set.fromList
  ["+", "-", "*", "/", "abs", "^", "sqrt", "mod", "log", "ln", "exp", "abs",
  "round", "ceiling", "floor"]

isComparison, isLogical, isArith :: Text -> Bool
isComparison s = Set.member s comparisonOperators
isLogical    s = Set.member s logicalOperators
isArith      s = Set.member s arithOperators

ofBasicOperators :: Text -> Either Text Text
ofBasicOperators s = if isBasic then Right s else Left s
  where
    isBasic = Set.member s
      (comparisonOperators <> logicalOperators <> arithOperators)

-- helper patterns
pattern NativeFunc :: forall a. Text -> Fun a
pattern NativeFunc f <- FNative _ f _ _

-- compileNode's Patterns

pattern AST_InlinedApp :: [AST a] -> AST a
pattern AST_InlinedApp body <- App _node (FDefun _ _ _ _ body _) _args

pattern AST_Let :: forall a. a -> [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_Let node bindings body = Binding node bindings body BindLet

pattern AST_BindSchema :: forall a. a -> [(Named a, AST a)] -> a -> [AST a] -> AST a
pattern AST_BindSchema node bindings schema body <- Binding node bindings body (BindSchema schema)

pattern AST_Var :: forall a. a -> AST a
pattern AST_Var var <- TC.Var var

pattern AST_Lit :: forall a. Literal -> AST a
pattern AST_Lit lit <- Prim _ (PrimLit lit)

pattern AST_NegativeVar :: forall a. a -> AST a
pattern AST_NegativeVar var <- App _ (NativeFunc "-") [AST_Var var]

pattern AST_NegativeLit :: forall a. Literal -> AST a
pattern AST_NegativeLit lit <- App _ (NativeFunc "-") [AST_Lit lit]

pattern AST_NFun :: forall a. a -> Text -> [AST a] -> AST a
pattern AST_NFun node fn args <- App node (NativeFunc fn) args

pattern AST_NFun_Basic :: forall a. Text -> [AST a] -> AST a
pattern AST_NFun_Basic fn args <-
  AST_NFun _ (ofBasicOperators -> Right fn) args

pattern AST_If :: forall a. a -> AST a -> AST a -> AST a -> AST a
pattern AST_If node cond then' else' <-
  App node (NativeFunc "if") [cond, then', else']

pattern AST_StringLit :: forall a. Text -> AST a
pattern AST_StringLit str <- AST_Lit (LString str)

pattern AST_Enforce :: forall a. a -> AST a -> AST a
pattern AST_Enforce node cond <-
  App node (NativeFunc "enforce") (cond:_rest)

pattern AST_ReadKeyset :: forall a. AST a -> AST a
pattern AST_ReadKeyset name <-
  App _node (NativeFunc "read-keyset") [name]

pattern AST_EnforceKeyset :: forall a. AST a -> AST a
pattern AST_EnforceKeyset ks <-
  App _node (NativeFunc "enforce-keyset") [ks] -- can be string or object

pattern AST_AddTime :: forall a. AST a -> AST a -> AST a
pattern AST_AddTime time seconds <- App _ (NativeFunc "add-time") [time, seconds]

pattern AST_Days :: forall a. AST a -> AST a
pattern AST_Days days <- App _ (NativeFunc "days") [days]

pattern AST_Hours :: forall a. AST a -> AST a
pattern AST_Hours hours <- App _ (NativeFunc "hours") [hours]

pattern AST_Minutes :: forall a. AST a -> AST a
pattern AST_Minutes minutes <- App _ (NativeFunc "minutes") [minutes]

pattern AST_WithRead
  :: Node
  -> Text
  -> AST Node
  -> [(Named Node, AST Node)]
  -> Node
  -> [AST Node]
  -> AST Node
pattern AST_WithRead node table key bindings schema body <-
  App node
      (NativeFuncSpecial "with-read" (AST_BindSchema _ bindings schema body))
      [ShortTableName table, key]

pattern AST_Bind
  :: Node
  -> AST Node
  -> [(Named Node, AST Node)]
  -> Node
  -> [AST Node]
  -> AST Node
pattern AST_Bind node object bindings schema body <-
  App node
      (NativeFuncSpecial "bind" (AST_BindSchema _ bindings schema body))
      [object]

-- pattern RawTableName :: Text -> AST Node
-- pattern RawTableName t <- Table (Node (TcId _ t _) _) _

pattern ShortTableName :: Text -> AST Node
pattern ShortTableName tn <- Table _node (Lang.TableName tn)

pattern NativeFuncSpecial :: forall a. Text -> AST a -> Fun a
pattern NativeFuncSpecial f bdy <- FNative _ f _ (Just (_,SBinding bdy))

pattern AST_Read :: Node -> Text -> AST Node -> AST Node
pattern AST_Read node tn key <- App node (NativeFunc "read") [ShortTableName tn, key]

pattern AST_ReadCols :: Node -> Text -> AST Node -> [AST Node] -> AST Node
pattern AST_ReadCols node tn key columns
  <- App node (NativeFunc "read") [ShortTableName tn, key, List _ columns]

pattern AST_At :: a -> AST a -> AST a -> AST a
pattern AST_At node colName obj <- App node (NativeFunc "at") [colName, obj]

pattern AST_Obj :: forall a. a -> [(AST a, AST a)] -> AST a
pattern AST_Obj objNode kvs <- Object objNode kvs
