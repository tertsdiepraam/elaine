module Elaine.Transform where

import Data.List (isSuffixOf)
import Data.Map (Map, (!))
import Elaine.AST
import Elaine.Ident (Ident (idText))

-- import Elaine.Eval (subst)
-- elabToHandle :: Program -> Program
-- elabToHandle = foldProgram elabToHandle'

-- elabToHandle' :: Expr -> Expr
-- elabToHandle' = \case
--   Elab e1 e2 -> Handle e1 e2
--   App (Var x) args
--     | isHigherOrder x ->
--         App (App (Var x) (map (Val . lam []) args)) []
--   Val (Elb (Elaboration _ _ clauses)) ->
--     let mapping params = zip params (map (\p -> App (Var p) []) params)
--         resume = Ident "resume" LocNone
--         f' (OperationClause x' params e) = OperationClause x' params (App (Var resume) [Val $ lam [] (subst (mapping params) e)])
--         clauses' = map f' clauses
--      in Val $
--           Hdl $
--             Handler
--               Nothing
--               clauses'
--   e -> e

makeElabExplicit :: Map Int [Ident] -> Program -> Program
makeElabExplicit m = foldProgram (makeElabExplicit' m)

makeElabExplicit' :: Map Int [Ident] -> Expr -> Expr
makeElabExplicit' elabIdents = \case
  ImplicitElab i e -> foldr (\f x -> f x) e (map (Elab . Var) $ elabIdents ! i)
  e -> e

desugarRec :: Program -> Program
desugarRec = (foldProgram desugarRecExpr) . (map desugarRecDec)

desugarRecDec :: Declaration -> Declaration
desugarRecDec (Declaration vis decType) = Declaration vis $ case decType of
  DecLet x Rec t e ->  DecLet x NotRec t $ letSub x e
  Module x decs -> Module x $ map desugarRecDec decs
  a -> a

desugarRecExpr :: Expr -> Expr
desugarRecExpr (Let (Just (x, Rec)) mt e1 e2) = Let (Just (x, NotRec)) mt (letSub x e1) e2
desugarRecExpr a = a

letSub :: Ident -> Expr -> Expr
letSub x e = Let (Just (x, NotRec)) Nothing
    (Val $ Fn $ Function [(x, Nothing)] Nothing e')
    e'
  where 
    replaceX (Var x') = if x == x' then App (Var x) [Var x] else Var x'
    replaceX a = a
    e' = foldExpr replaceX e

foldProgram :: (Expr -> Expr) -> Program -> Program
foldProgram f = map (foldDec f)

foldDec :: (Expr -> Expr) -> Declaration -> Declaration
foldDec f (Declaration vis decType) = Declaration vis $ case decType of
  DecLet x r t e -> DecLet x r t (foldExpr f e)
  Module x decs -> Module x (foldProgram f decs)
  x -> x

foldExpr :: (Expr -> Expr) -> Expr -> Expr
foldExpr f' e = f' (foldInner e)
  where
    foldInner = \case
      Elab e1 e2 -> Elab (f e1) (f e2)
      ImplicitElab i e1 -> ImplicitElab i (f e1)
      App e1 args -> App (f e1) (map f args)
      Tuple es -> Tuple (map f es)
      If e1 e2 e3 -> If (f e1) (f e2) (f e3)
      Handle e1 e2 -> Handle (f e1) (f e2)
      Match e1 arms -> Match (f e1) (map (\(MatchArm p e') -> MatchArm p (f e')) arms)
      Var x -> Var x
      Let x t e1 e2 -> Let x t (f e1) (f e2)
      Val v -> Val $ foldVal f' v
      where
        f = foldExpr f'

foldVal :: (Expr -> Expr) -> Value -> Value
foldVal f = \case
  Fn fun ->
    Fn $ foldFun f fun
  Hdl (Handler fun clauses) ->
    Hdl $
      Handler
        (fmap (foldFun f) fun)
        (foldClauses clauses)
  Elb (Elaboration from to clauses) ->
    Elb $
      Elaboration from to (foldClauses clauses)
  x -> x
  where
    foldClauses = map (\(OperationClause x params e) -> OperationClause x params (foldExpr f e))

foldFun :: (Expr -> Expr) -> Function -> Function
foldFun f (Function params ret body) = Function params ret (foldExpr f body)

isAlgebraic :: Ident -> Bool
isAlgebraic = not . isHigherOrder

isHigherOrder :: Ident -> Bool
isHigherOrder x = "!" `isSuffixOf` idText x
