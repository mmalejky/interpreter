{-# LANGUAGE LambdaCase #-}

module Interpreter ( evalProgram ) where

import Prelude hiding (id)
import System.IO (hPutStrLn, stderr)
import Data.Maybe ( fromJust )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except

import AbsBarraquito

data Value     = IntV  { getInt  :: Integer } |
                 BoolV { getBool :: Bool    } |
                 StrV String

type Eval      = ExceptT String IO
type VarEnv    = Map Ident Loc
type FunEnv    = Map Ident Proc
type Proc      = [Loc] -> ContExpr -> Store -> Eval Ans
type Loc       = Int
type Store     = Map Loc Value
type Ans       = Store
type ContExpr  = Value  -> Store -> Eval Ans
type ContStmt  = VarEnv -> Store -> Eval Ans
type ContFnDef = FunEnv -> Eval Ans

instance Show Value where
  show (IntV x)  = show x
  show (StrV x)  = x
  show (BoolV x) = show x

class Op a where
  evalOp :: Value -> a -> Value -> Eval Value

instance Op (AddOp' a) where
  evalOp v1 op v2 = case op of
    Plus _  -> return $ IntV $ (getInt v1) + (getInt v2)
    Minus _ -> return $ IntV $ (getInt v1) - (getInt v2)

instance Op (MulOp' a) where
  evalOp v1 op v2 = case op of
    Times _ -> return $ IntV $ (getInt v1) * (getInt v2)
    Div _   -> case getInt v2 of
      0 -> throwError "Dividing by zero"
      _ -> return $ IntV $ (getInt v1) `div` (getInt v2)
    Mod _   -> case getInt v2 of
      0 -> throwError "Dividing modulo by zero"
      _ -> return $ IntV $ (getInt v1) `mod` (getInt v2)

instance Op (RelOp' a) where
  evalOp v1 op v2 = case op of
    LTH _ -> return $ BoolV $ (getInt v1) <  (getInt v2)
    LE _  -> return $ BoolV $ (getInt v1) <= (getInt v2)
    GTH _ -> return $ BoolV $ (getInt v1) >  (getInt v2)
    GE _  -> return $ BoolV $ (getInt v1) >= (getInt v2)
    EQU _ -> return $ BoolV $ (getInt v1) == (getInt v2)
    NE _  -> return $ BoolV $ (getInt v1) /= (getInt v2)

data BoolOp = And | Or
instance Op BoolOp where
  evalOp v1 op v2 = case op of
    And -> return $ BoolV $ (getBool v1) && (getBool v2)
    Or  -> return $ BoolV $ (getBool v1) || (getBool v2)

get :: (Ord a) => Map a b -> a -> b
get m a = fromJust $ Map.lookup a m

newLoc :: Map Loc b -> Int
newLoc m = if Map.null m then 0 else 1 + fst (Map.findMax m)

evalExpr :: Expr -> VarEnv -> FunEnv -> ContExpr -> Store -> Eval Ans
evalExpr expr pV pF cE s = case expr of
  EVar _ id       -> cE (get s (get pV id)) s
  ELitInt _ n     -> cE (IntV n) s
  ELitTrue _      -> cE (BoolV True) s
  ELitFalse _     -> cE (BoolV False) s
  EApp _ fid es   -> evalApp (newLoc s) [] es s where
    evalApp :: Int -> [Loc] -> [Expr] -> Store -> Eval Ans
    evalApp next argLocs ((EVar _ vid):exprs) s' =
      evalApp next (argLocs ++ [get pV vid]) exprs s'
    evalApp next argLocs (e:exprs) s'            =
      evalExpr e pV pF (\v s'' ->
        evalApp (next+1) (argLocs ++ [next]) exprs (Map.insert next v s'')) s'
    evalApp _ argLocs [] s'                      = (get pF fid) argLocs cE s'
  EString _ str   -> cE (StrV str) s
  Neg _ e         -> evalExpr e pV pF (\(IntV n)  s'-> cE (IntV $ negate n) s') s
  Not _ e         -> evalExpr e pV pF (\(BoolV b) s'-> cE (BoolV $ not b)   s') s
  EMul _ e1 op e2 -> evalExpr e1 pV pF (\v1 s' -> evalExpr e2 pV pF (\v2 s'' ->
    evalOp v1 op  v2 >>= (\v -> cE v s'')) s') s
  EAdd _ e1 op e2 -> evalExpr e1 pV pF (\v1 s' -> evalExpr e2 pV pF (\v2 s'' ->
    evalOp v1 op  v2 >>= (\v -> cE v s'')) s') s
  ERel _ e1 op e2 -> evalExpr e1 pV pF (\v1 s' -> evalExpr e2 pV pF (\v2 s'' ->
    evalOp v1 op  v2 >>= (\v -> cE v s'')) s') s
  EAnd _ e1 e2    -> evalExpr e1 pV pF (\v1 s' -> evalExpr e2 pV pF (\v2 s'' ->
    evalOp v1 And v2 >>= (\v -> cE v s'')) s') s
  EOr _ e1 e2     -> evalExpr e1 pV pF (\v1 s' -> evalExpr e2 pV pF (\v2 s'' ->
    evalOp v1 Or  v2 >>= (\v -> cE v s'')) s') s

evalStmt :: Stmt -> VarEnv -> FunEnv -> ContExpr -> ContStmt -> ContStmt ->
  ContStmt -> Store -> Eval Ans
evalStmt stmt pV pF cE cB cC cS s = case stmt of
  Ass _ id e         -> evalExpr e pV pF (\v s'->
    cS pV (Map.insert (get pV id) v s')) s
  Decl _ _ [(Init _ id e)] -> evalExpr e pV pF (\v s'->
    cS (Map.insert id nLoc pV) (Map.insert nLoc v s')) s where
      nLoc = newLoc s
  Decl pos t (item:rest) -> evalStmt (Decl pos t [item]) pV pF cE cB cC (\pV' s' ->
    evalStmt (Decl pos t rest) pV' pF cE cB cC cS s') s
  Decl _ _ []        -> error "Empty declaration list"
  -- Empty declaration list would not pass the parsing phase
  Print _ e          -> evalExpr e pV pF (\v s'-> do liftIO (putStrLn (show v))
                                                     cS pV s') s
  Empty _            -> cS pV s
  BStmt _ b          -> evalBlock b pV pF cE cB cC cS s
  Incr _ id          -> cS pV (Map.adjust (IntV . (1+) . getInt) (get pV id) s)
  Decr _ id          -> cS pV (Map.adjust (IntV . ((-1)+) . getInt) (get pV id) s)
  Break _            -> cB pV s
  Continue _         -> cC pV s
  Ret _ e            -> evalExpr e pV pF cE s
  Cond _ e block     -> evalExpr e pV pF (\(BoolV b) s' -> if b
                            then evalBlock block pV pF cE cB cC cS s'
                            else cS pV s') s
  CondElse _ e b1 b2 -> evalExpr e pV pF (\(BoolV b) s' -> if b
                            then evalBlock b1 pV pF cE cB cC cS s'
                            else evalBlock b2 pV pF cE cB cC cS s') s
  While _ e block    -> evalExpr e pV pF (\(BoolV b) s' -> if b
                            then evalBlock block pV pF cE cS (\pV' ->
                              evalStmt stmt pV' pF cE cB cC cS) (\pV' ->
                                evalStmt stmt pV' pF cE cB cC cS) s'
                            else cS pV s') s
  SExp _ e           -> evalExpr e pV pF (\_ s' -> cS pV s') s

evalStmts :: [Stmt] -> VarEnv -> FunEnv -> ContExpr -> ContStmt -> ContStmt ->
  ContStmt -> Store -> Eval Ans
evalStmts stmts pV pF cE cB cC cS s = case stmts of
  stmt:rest -> evalStmt stmt pV pF cE cB cC (\pV' s' ->
    evalStmts rest pV' pF cE cB cC cS s') s
  []        -> cS pV s

evalFnDef :: FnDef -> VarEnv -> FunEnv -> ContFnDef -> Store -> Eval Ans
evalFnDef (FnDef _ _ fid args block) pV pF cF _ =
  cF (Map.insert fid (fix phi) pF) where
    phi proc = tempFun where
      tempFun locs c' s' =
        evalBlock block newpV newpF c' cBadBreak cBadContinue cNoReturn newS where
          newpV = Map.union pV' pV
          newpF = Map.insert fid proc pF
          newS = Map.union s'' s'
          s'' = Map.fromList $ zip (newLocs s' normArgs) (map (\(_, loc) ->
            get s' loc) normArgs)
          pV' = Map.fromList $ refArgs ++ (zip (map fst normArgs) (newLocs s' normArgs))
          normArgs = map (\((Arg _ _ id), loc) -> (id, loc)) $ filter (\case
            (Arg _ _ _, _) -> True
            _              -> False ) (zip args locs)
          refArgs = map (\((ArgRef _ _ id), loc) -> (id, loc)) $ filter (\case
            (ArgRef _ _ _, _) -> True
            _                 -> False ) (zip args locs)
          newLocs m args' = [(newLoc m)..((newLoc m)+((length args') - 1))]

evalFnDefs :: [FnDef] -> VarEnv -> FunEnv -> ContFnDef -> Store -> Eval Ans
evalFnDefs fndefs pV pF cF s = case fndefs of
  fndef:rest -> evalFnDef fndef pV pF (\pF' -> evalFnDefs rest pV pF' cF s) s
  []         -> cF pF

evalBlock :: Block -> VarEnv -> FunEnv -> ContExpr -> ContStmt -> ContStmt ->
  ContStmt -> Store -> Eval Ans
evalBlock (Block _ fs stmts) pV pF cE cB cC cS s =
  evalFnDefs fs pV pF (\pF' -> evalStmts stmts pV pF' cE cB cC cS s) s

cBadReturn :: ContExpr
cBadBreak, cBadContinue, cIdStmt, cNoReturn :: ContStmt
cBadReturn _ _   = throwError "Return ouside of function"
-- Return outside of function is statically checked
cNoReturn _ _    = throwError "No return from non-void function"
cBadBreak _ _    = throwError "Break ouside of loop"
cBadContinue _ _ = throwError "Continue ouside of loop"
cIdStmt _ s      = return s

evalProgram :: Program -> IO ()
evalProgram (Program pos fs stmts) = do
  r <- runExceptT progEval
  reportResult r where
    progEval  = evalBlock progBlock eM eM cBadReturn cBadBreak cBadContinue cIdStmt eM
    progBlock = Block pos fs stmts
    eM        = Map.empty

reportResult :: Either String Ans -> IO ()
reportResult (Right _) = return ()
reportResult (Left err)  = do hPutStrLn stderr "> Computation failed with error:"
                              hPutStrLn stderr err
