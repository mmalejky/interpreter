{-# LANGUAGE LambdaCase #-}

module TypeChecker ( checkProgram ) where

import Prelude hiding (id)
import AbsBarraquito
import Data.Map (Map)
import qualified Data.Map as Map

type ExpctType = Type
type GotType   = Type
type RetType   = Maybe Type
type Err       = Either String
type Env       = Map Ident Type
type VarEnv    = Env
type FunEnv    = Env

baseInt, baseBool :: Type
baseInt  = Int BNFC'NoPosition
baseBool = Bool BNFC'NoPosition

isInt, isBool :: Type -> Err Type
isInt    = matchType baseInt
isBool   = matchType baseBool

showPos :: BNFC'Position -> String
showPos Nothing            = "nowhere"
showPos (Just (line, col)) = "line " ++ show line ++ ", column " ++ show col

showType :: Type -> String
showType t = case t of
  Int _        -> "Integer"
  Str _        -> "String"
  Bool _       -> "Boolean"
  Fun _ rt ats -> show (map showType ats) ++ "->" ++ showType rt

checkProgram :: Program -> Err Bool
checkProgram (Program pos fs stmts) =
  checkBlock (Block pos fs stmts) Nothing Map.empty Map.empty

matchType :: ExpctType -> GotType -> Err Type
matchType (Int _) t@(Int _)   = return t
matchType (Str _) t@(Str _)   = return t
matchType (Bool _) t@(Bool _) = return t
matchType f1@(Fun _ t1 ts1) f2@(Fun fCallPos t2 ts2) = do
  matchType t1 t2 >> matchArgs fCallPos ts1 ts2 >> return f2
  where
    matchArgs :: BNFC'Position -> [Type] -> [Type] -> Err Bool
    matchArgs pos args1 args2 = if length args1 == length args2
      then do _ <- sequenceA (zipWith matchType ts1 ts2)
              return True
      else Left $ errStr ++ "\n" ++ typesCompStr where
        errStr       = "Wrong number of arguments in function call at " ++ showPos pos
        typesCompStr = "Expected " ++ showType f1 ++ ", but got " ++ showType f2
matchType t1 t2             = Left $ errStr ++ "\n" ++ typesCompStr where
  errStr       = "Types do not match at " ++ showPos (hasPosition t2)
  typesCompStr = "Expected " ++ showType t1 ++ ", but got " ++ showType t2

operMatchType :: Expr -> Expr -> ExpctType -> VarEnv -> FunEnv -> Err Type
operMatchType e1 e2 t pV pF =
  getType e2 pV pF >>= matchType t >> getType e1 pV pF >>= matchType t

getType :: Expr -> VarEnv -> FunEnv -> Err Type
getType expr pV pF = case expr of
  EVar _ id      -> pV @! id
  EApp pos id exps -> do
    types <- sequenceA $ map (\e -> getType e pV pF) exps
    f <- pF @! id
    _ <- matchType f (Fun pos (fRetType f) types)
    return $ fRetType f
    where
      fRetType (Fun _ t _) = t
      fRetType _           = error "Return type of non-function was queried"
  Neg _ e        -> getType e pV pF >>= isInt
  Not _ e        -> getType e pV pF >>= isBool
  ELitInt pos _  -> return (Int pos)
  ELitTrue pos   -> return (Bool pos)
  ELitFalse pos  -> return (Bool pos)
  EString pos _  -> return (Str pos)
  EMul _ e1 _ e2 -> operMatchType e1 e2 baseInt pV pF
  EAdd _ e1 _ e2 -> operMatchType e1 e2 baseInt pV pF
  EAnd _ e1   e2 -> operMatchType e1 e2 baseBool pV pF
  EOr  _ e1   e2 -> operMatchType e1 e2 baseBool pV pF
  ERel _ e1 _ e2 -> operMatchType e1 e2 baseInt pV pF >> return baseBool

fType :: FnDef -> Type
fType (FnDef pos retType _ args _) = Fun pos retType (map argType args) where
  argType (Arg _ t _)    = t
  argType (ArgRef _ t _) = t

checkBlock :: Block -> Maybe Type -> VarEnv -> FunEnv -> Err Bool
checkBlock (Block pos (f:fs) stmts) retType pV pF = case f of
  FnDef _ fRetType fId args fBlock -> do
    _ <- checkBlock (Block pos fs stmts) retType pV pF'
    checkBlock fBlock (Just fRetType) pV' pF'
    where
      pF' = Map.insert fId (fType f) pF
      pV' = Map.union argspV pV
      argspV = Map.fromList $ map argTypeId args
      argTypeId (Arg _ t id)    = (id, t)
      argTypeId (ArgRef _ t id) = (id, t)
checkBlock (Block pos [] (stmt:stmts)) retType pV pF = do
  pV' <- checkStmt stmt retType pV pF
  checkBlock (Block pos [] stmts) retType pV' pF
checkBlock (Block _ [] []) _ _ _ = return True

(@!) :: Env -> Ident -> Err Type
(@!) env id@(Ident str) = case Map.lookup id env of
  Just t -> return t
  _      -> Left $ "Identifier " ++ str ++ " not in scope"

checkStmt :: Stmt -> RetType -> VarEnv -> FunEnv -> Err VarEnv
checkStmt stmt rT pV pF = case stmt of
  Decl _ t inits     -> checkDecl t inits pV
  Ret pos e          -> case rT of { Just t -> gT e >>= matchType t >> ret ; _ -> badRet pos }
  Ass _ id e         -> do varType <- pV @! id
                           exprType <- gT e
                           matchType varType exprType >> ret
  CondElse _ e b1 b2 -> gT e >>= isBool >> cB b1 >> cB b2 >> ret
  Cond _ e b         -> gT e >>= isBool >> cB b >> ret
  While _ e b        -> gT e >>= isBool >> cB b >> ret
  Incr _ id          -> pV @! id >>= isInt >> ret
  Decr _ id          -> pV @! id >>= isInt >> ret
  SExp _ e           -> gT e >> ret
  Print _ e          -> gT e >> ret
  BStmt _ b          -> cB b >> ret
  Empty _            -> ret
  Break _            -> ret
  Continue _         -> ret
  where
    checkDecl :: Type -> [Item] -> VarEnv -> Err VarEnv
    checkDecl t ((Init _ id e):inits) pV' = gT e >>= matchType t >> checkDecl t inits (Map.insert id t pV')
    checkDecl _ [] pV'                    = return pV'
    badRet pos = Left $ "Return outside of the function at " ++ showPos pos
    ret  = return pV :: Err VarEnv
    cB b = checkBlock b rT pV pF
    gT e = getType e pV pF
