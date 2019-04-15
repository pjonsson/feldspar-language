module Feldspar.Core.Middleend.OptimizeUntyped ( optimize ) where

import Data.Map.Strict (Map, empty)
import Feldspar.Core.UntypedRepresentation

-- | General simplification. Could in theory be done at earlier stages.
optimize :: UntypedFeld -> UntypedFeld
optimize = go empty . go empty

go :: Map k v -> UntypedFeld -> UntypedFeld
go _ e@(In Variable{}) = e
go env (In (Lambda v e)) = In (Lambda v (go env e))
go env (In (LetFun (s, f, e1) e2)) = In (LetFun (s, f, go env e1) (go env e2))
go _ l@(In Literal{}) = l

go env (In (App Let _ [e1, In (Lambda x body)]))
 | (In Variable{}) <- e1 -- let x = y in e ==> [y/x]e
 = go env $ subst e1 x body
 | linear x body
 = go env $ subst e1 x body

go env (In (App Add _ [e1, e2]))
 | zero e1 = go env e2
 | zero e2 = go env e1

go env (In (App Sub _ [e1, e2]))
 | zero e2 = go env e1

go env (In (App Mul _ [e1, e2]))
 | zero e1 = e1
 | zero e2 = e2
 | one e1  = go env e2
 | one e2  = go env e1

go env (In (App Div _ [e1, e2]))
 | one e2  = go env e1

-- Basic constant folder.
go _ e@(In (App p _ [In (Literal l1), In (Literal l2)]))
  | p `elem` [Add, Sub, Mul]
  = constFold e p l1 l2

-- For 1 (\v -> body) ==> [0/v]body
go env (In (App p _ [In (Literal (LInt s sz 1)), In (Lambda v body)]))
  | p `elem` [For, EparFor]
  = go env $ subst (In (Literal (LInt s sz 0))) v body

-- Create a 1 element long array (frequent with MultiDim) and immediately select that
-- element. Can e seen in the metrics test in feldspar-compiler.
-- (RunMutableArray (Bind (NewArr_ 1)
--                        (\v3 -> Then (SetArr v3 0 e3) (Return v3)))) ! 0
go env (In (App GetIx _ [arr, In (Literal (LInt _ _ 0))]))
 | (In (App RunMutableArray _ [In (App Bind _ [In (App NewArr_ _ [l]), e'])])) <- arr
 , one l
 , (In (Lambda v1 (In (App Then _  [sarr, ret])))) <- e'
 , (In (App SetArr _ [In (Variable v3), In (Literal (LInt _ _ 0)), e3])) <- sarr
 , (In (App Return _ [In (Variable v2)])) <- ret
 , v1 == v2
 , v1 == v3 = go env e3

-- Same rule as previous rule but with Elements as backing write.
go env (In (App GetIx _ [arr, In (Literal (LInt _ _ n))]))
 | In (App EMaterialize _ [In Literal{}, e@(In (App EPar _ _))]) <- arr
 , Just e3 <- grabWrite n e = go env e3

-- Tuple selections, 1..15. Deliberately avoiding take 1 . drop k which will
-- result in funny things with broken input.
go env (In (App Sel1 _  [In (App Tup _ (e:_))]))
  = go env e
go env (In (App Sel2 _  [In (App Tup _ (_:e:_))]))
  = go env e
go env (In (App Sel3 _  [In (App Tup _ (_:_:e:_))]))
  = go env e
go env (In (App Sel4 _  [In (App Tup _ (_:_:_:e:_))]))
  = go env e
go env (In (App Sel5 _  [In (App Tup _ (_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel6 _  [In (App Tup _ (_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel7 _  [In (App Tup _ (_:_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel8 _  [In (App Tup _ (_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel9 _  [In (App Tup _ (_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel10 _ [In (App Tup _ (_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel11 _ [In (App Tup _ (_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel12 _ [In (App Tup _ (_:_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel13 _ [In (App Tup _ (_:_:_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel14 _ [In (App Tup _ (_:_:_:_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (In (App Sel15 _ [In (App Tup _ (_:_:_:_:_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e

-- Fallthrough.
go env (In (App p t es)) = In (App p t $ map (go env) es)

linear :: Var -> UntypedFeld -> Bool
linear v e = count v e <= 1

-- | Occurence counter. Cares about dynamic behavior, so loops count as a lot.
count :: Var -> UntypedFeld -> Integer
count v (In (Variable v')) = if v == v' then 1 else 0
count v e@(In (Lambda v' _))
  | v == v' || v `notElem` fv e = 0
  | otherwise                  = 100 -- Possibly inside loop
count v (In (LetFun (_, _, e1) e2)) = count v e1 + count v e2
count _ (In Literal{}) = 0
count v (In (App Let _ [e1, In (Lambda x body)]))
  | v == x    = count v e1
  | otherwise = count v e1 + count v body
count _ (In (App Await _ _)) = 100 -- Do not inline.
count _ (In (App NoInline _ _)) = 100 -- Do not inline.
count v (In (App _ _ es)) = sum $ map (count v) es

-- TODO: Improve precision of switch.

-- | Is this a literal zero.
zero :: UntypedFeld -> Bool
zero (In (Literal (LInt    _ _ 0))) = True
zero (In (Literal (LFloat      0))) = True
zero (In (Literal (LDouble     0))) = True
zero _                              = False

-- | Is this a literal one.
one :: UntypedFeld -> Bool
one (In (Literal (LInt    _ _ 1))) = True
one (In (Literal (LFloat      1))) = True
one (In (Literal (LDouble     1))) = True
one _                              = False

constFold :: UntypedFeld -> Op -> Lit -> Lit -> UntypedFeld
constFold _ Add (LInt sz n n1) (LInt _ _ n2) = In (Literal (LInt sz n (n1 + n2)))
constFold _ Sub (LInt sz n n1) (LInt _ _ n2) = In (Literal (LInt sz n (n1 - n2)))
constFold _ Mul (LInt sz n n1) (LInt _ _ n2) = In (Literal (LInt sz n (n1 * n2)))
constFold e _ _ _ = e

-- | Scan an Epar/Ewrite-nest and return the element written to a position.
grabWrite :: Integer -> UntypedFeld -> Maybe UntypedFeld
grabWrite n (In (App EPar _ [e1,e2]))
 | Nothing <- r1 = grabWrite n e2
 | otherwise = r1
   where r1 = grabWrite n e1
grabWrite n (In (App EWrite _ [In (Literal (LInt _ _ k)), e]))
 | k == n = Just e
grabWrite _ _ = Nothing
