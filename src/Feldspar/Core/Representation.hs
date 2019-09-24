{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

--
-- Copyright (c) 2019, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Feldspar.Core.Representation
  ( Var(..)
  , VarId(..)
  , TypeF(..)
  , AExpr(..)
  , Info(..)
  , Expr(..)
  , exprType
  , ExprCtx(..)
  , toAExpr
  , (:->)
  , EqBox(..)
  , Op(..)
  , fvi
  , fviR
  , CBind(..)
  , bvId
  , fviB
  , showRhs
  , BindEnv(..)
  , lookupBE
  , extendBE
  , mkLets
  , sharable
  , legalToShare
  , goodToShare
  ) where

import Language.Syntactic.Constructs.Binding (VarId (..))
import Feldspar.Core.Types(Type(typeRep), TypeF(..), TypeRep(..), Length, Index, IntN, Size(..), Elements, FVal, Mut, AnySize, MArr, Par, IV)
import Feldspar.Range

import qualified Data.ByteString.Char8 as B
import Data.Typeable (Typeable, (:~:)(Refl), eqT)
import Data.Hash (Hashable)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Bits (Bits)
import Data.Complex (Complex)
import Data.IORef (IORef)

import Feldspar.Core.Tuple (Tuply(..))

infixr :->
infixl 5 :@
infix 1 :&

data Var a = Var { varNum :: VarId
                 , varName :: B.ByteString
                 }

instance Eq (Var a) where
  v1 == v2 = varNum v1 == varNum v2

instance Show (Var a) where
  show (Var n _) = "v" ++ show n

-- | Full expressions always have types of the form 'Expr (Full t)' for some 't' to
--   distinguish them from partial applications of operators.
type Full a = a

-- We currently do not use the type constructor :->
type a :-> b = a -> b

type FExpr a = Expr (Full a)

-- | The type of information, for instance range information. Currently empty.
data Info a = Info
  deriving (Eq, Show)

-- | Adding default info to an Expr
toAExpr :: FExpr a -> AExpr a
toAExpr e = Info :& e

-- | Constructing an annotation for a Lambda
funInfo :: Var a -> AExpr e -> Info (a -> e)
funInfo v e = Info

-- | Annotated expression, that is, an expression together with extra information,
--   for instance from a program analysis.
data AExpr a = (:&) {aeInfo :: Info a, aeExpr :: Expr (Full a)}
  deriving (Eq)

instance Show (AExpr a) where
  show = showAExpr 0

showAExpr :: Int -> AExpr a -> String
showAExpr n (_ :& e) = showExpr n e

type LiteralType a = (Show a, Eq a, Typeable a, Hashable a)
type ExprCtx a = (TypeF a)

{- | The main expression type.
     Applications always have an operator at the left end, and are never annotated.

     Note that an operator itself is not a full expression unless its type is of
     the form 'Full t' for some 't'.
-}
data Expr a where
  Literal  :: LiteralType a          => a -> Expr (Full a)
  Operator ::                           Op a -> Expr a
  Variable ::                           Var a -> Expr (Full a)
  (:@)     :: ExprCtx a              => Expr (a -> b) -> AExpr a -> Expr b
  Lambda   :: (ExprCtx a, ExprCtx b) => Var a -> AExpr b -> Expr (Full (a -> b))

exprType :: TypeF a => Expr a -> TypeRep a
exprType _ = typeRepF

literal :: LiteralType a => a -> AExpr a
literal x = Info :& Literal x

instance Show (Expr a) where
  show = showExpr 0

showExpr :: Int -> Expr a -> String
showExpr _ (Literal l)   = show l
showExpr _ (Operator op) = show op
showExpr _ (Variable v)  = "v" ++ show (varNum v)
showExpr n (f :@ e)      = showExpr n f ++
                          "\n" ++ replicate (n+2) ' ' ++
                          showAExpr (n+2) e
showExpr n (Lambda v e) = "\\ " ++ show v ++ " ->\n  " ++ showAExpr (n+2) e

instance Typeable a => Eq (Expr a) where
  Literal l == Literal r = l == r
  Operator op1 == Operator op2 = op1 == op2
  Variable v1 == Variable v2 = v1 == v2
  ((f1 :: Expr (a1 -> b1)) :@ e1) == ((f2 :: Expr (a2 -> b2)) :@ e2)
        = case eqT :: Maybe ((a1,b1) :~: (a2,b2)) of
            Nothing -> False
            Just Refl -> f1 == f2 && e1 == e2
  Lambda v1 e1 == Lambda v2 e2 = v1 == v2 && e1 == e2
  _ == _ = False

-- | A box which makes its contents equal to everything else with the same type
newtype EqBox a = EqBox {unEqBox :: a}

instance Eq (EqBox a) where
  x == y = True

instance Show (EqBox a) where
  show x = "Box"

-- | The main data type for built-in operators as well as let and conditional
--   constructs.
data Op a where
    -- | Array
    Parallel   :: Type a => Op (Length :-> (Index -> a) :-> Full [a])
    Sequential :: (Type a, Type st) =>
                  Op (Length :-> st :-> (Index -> st -> (a,st)) :-> Full [a])
    Append     :: Type a => Op ([a] :-> [a] :-> Full [a])
    GetIx      :: Type a => Op ([a] :-> Index :-> Full a)
    SetIx      :: Type a => Op ([a] :-> Index :-> a :-> Full [a])
    GetLength  :: Type a => Op ([a] :-> Full Length)
    SetLength  :: Type a => Op (Length :-> [a] :-> Full [a])

    -- | Binding
    Let :: Op (a :-> (a -> b) :-> Full b)

    -- | Bits
    BAnd          :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    BOr           :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    BXor          :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    Complement    :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :->       Full a)

    Bit           :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (Index :->       Full a)
    SetBit        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    ClearBit      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    ComplementBit :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    TestBit       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full Bool)

    ShiftLU       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    ShiftRU       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    ShiftL        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> IntN  :-> Full a)
    ShiftR        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> IntN  :-> Full a)
    RotateLU      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    RotateRU      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    RotateL       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> IntN  :-> Full a)
    RotateR       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> IntN  :-> Full a)
    ReverseBits   :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :->           Full a)

    BitScan       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Full Index)
    BitCount      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Full Index)

    -- | Complex
    MkComplex :: (Type a, RealFloat a) => Op (a :-> a :-> Full (Complex a))
    RealPart  :: (Type a, RealFloat a) => Op (Complex a :-> Full a)
    ImagPart  :: (Type a, RealFloat a) => Op (Complex a :-> Full a)
    Conjugate :: (Type a, RealFloat a) => Op (Complex a :-> Full (Complex a))
    MkPolar   :: (Type a, RealFloat a) => Op (a :-> a :-> Full (Complex a))
    Magnitude :: (Type a, RealFloat a) => Op (Complex a :-> Full a)
    Phase     :: (Type a, RealFloat a) => Op (Complex a :-> Full a)
    Cis       :: (Type a, RealFloat a) => Op (a :-> Full (Complex a))

    -- | Condition
    Condition  ::                      Op (Bool :-> a :-> a :-> Full a)

    -- | Conversion
    F2I     :: (Type a, Integral a, RealFloat b)                     => Op (b :-> Full a)
    I2N     :: (Type a, Type b, Integral a, Num b, Size a ~ Range a) => Op (a :-> Full b)
    B2I     :: (Type a, Integral a)                                  => Op (Bool  :-> Full a)
    Round   :: (Type a, Integral a, RealFloat b)                     => Op (b :-> Full a)
    Ceiling :: (Type a, Integral a, RealFloat b)                     => Op (b :-> Full a)
    Floor   :: (Type a, Integral a, RealFloat b)                     => Op (b :-> Full a)

    -- | Elements
    EMaterialize :: Type a => Op (Length :-> Elements a :-> Full [a])
    EWrite       :: Type a => Op (Index :-> a :-> Full (Elements a))
    ESkip        :: Type a => Op (Full (Elements a))
    EPar         :: Type a => Op (Elements a :-> Elements a :-> Full (Elements a))
    EparFor      :: Type a => Op (Length :-> (Index -> Elements a) :-> Full (Elements a))

    -- | Eq
    Equal    :: (Type a, Eq a) => Op (a :-> a :-> Full Bool)
    NotEqual :: (Type a, Eq a) => Op (a :-> a :-> Full Bool)

    -- | Error
    Undefined :: Type a => Op (Full a)
    Assert    :: Type a => String -> Op (Bool :-> a :-> Full a)

    -- FFI
    -- ForeignImport :: (Type (DenResult a))
    --              => String -> Denotation a -> Op a

    -- | Floating
    Pi      :: (Type a, Floating a) => Op (Full a)
    Exp     :: (Type a, Floating a) => Op (a :-> Full a)
    Sqrt    :: (Type a, Floating a) => Op (a :-> Full a)
    Log     :: (Type a, Floating a) => Op (a :-> Full a)
    Pow     :: (Type a, Floating a) => Op (a :-> a :-> Full a)
    LogBase :: (Type a, Floating a) => Op (a :-> a :-> Full a)
    Sin     :: (Type a, Floating a) => Op (a :-> Full a)
    Tan     :: (Type a, Floating a) => Op (a :-> Full a)
    Cos     :: (Type a, Floating a) => Op (a :-> Full a)
    Asin    :: (Type a, Floating a) => Op (a :-> Full a)
    Atan    :: (Type a, Floating a) => Op (a :-> Full a)
    Acos    :: (Type a, Floating a) => Op (a :-> Full a)
    Sinh    :: (Type a, Floating a) => Op (a :-> Full a)
    Tanh    :: (Type a, Floating a) => Op (a :-> Full a)
    Cosh    :: (Type a, Floating a) => Op (a :-> Full a)
    Asinh   :: (Type a, Floating a) => Op (a :-> Full a)
    Atanh   :: (Type a, Floating a) => Op (a :-> Full a)
    Acosh   :: (Type a, Floating a) => Op (a :-> Full a)

    -- | Fractional
    DivFrac :: (Type a, Fractional a) => Op (a :-> a :-> Full a)

    -- | Future
    MkFuture :: Type a => Op (a :-> Full (FVal a))
    Await    :: Type a => Op (FVal a :-> Full a)

    -- | Integral
    Quot :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    Rem  :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    Div  :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    Mod  :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    IExp :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)

    -- | Logic
    And :: Op (Bool :-> Bool :-> Full Bool)
    Or  :: Op (Bool :-> Bool :-> Full Bool)
    Not :: Op (Bool :->          Full Bool)

    -- | Loop
    ForLoop   :: Type a => Op (Length :-> a :-> (Index -> a -> a) :-> Full a)
    WhileLoop :: Type a => Op (a :-> (a -> Bool) :-> (a -> a) :-> Full a)

    -- | Mutable
    Run :: Type a => Op (Mut a :-> Full a)

    -- | MutableArray
    NewArr    :: Type a => Op (Length :-> a :-> Full (Mut (MArr a)))
    NewArr_   :: Type a => Op (Length :-> Full (Mut (MArr a)))
    GetArr    :: Type a => Op (MArr a :-> Index :-> Full (Mut a))
    SetArr    :: Op (MArr a :-> Index :-> a :-> Full (Mut ()))
    ArrLength :: Op (MArr a :-> Full (Mut Length))

    -- | MutableToPure
    RunMutableArray :: Type a => Op (Mut (MArr a) :-> Full [a])
    WithArray       :: Type b => Op (MArr a :-> ([a] -> Mut b) :-> Full (Mut b))

    -- | MutableReference
    NewRef :: Type a => Op (a :-> Full (Mut (IORef a)))
    GetRef :: Type a => Op (IORef a :-> Full (Mut a))
    SetRef :: Type a => Op (IORef a :-> a :-> Full (Mut ()))
    ModRef :: Type a => Op (IORef a :-> (a -> a) :-> Full (Mut ()))

    -- | NoInline
    NoInline :: (Type a) => Op (a :-> Full a)

    -- | Num
    Abs  :: (Type a, Num a, Num (Size a)) => Op (a :-> Full a)
    Sign :: (Type a, Num a, Num (Size a)) => Op (a :-> Full a)
    Add  :: (Type a, Num a, Num (Size a)) => Op (a :-> a :-> Full a)
    Sub  :: (Type a, Num a, Num (Size a)) => Op (a :-> a :-> Full a)
    Mul  :: (Type a, Num a, Num (Size a)) => Op (a :-> a :-> Full a)

    -- | Ord
    LTH :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full Bool)
    GTH :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full Bool)
    LTE :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full Bool)
    GTE :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full Bool)
    Min :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full a)
    Max :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full a)

    -- | Par
    ParRun    :: Type a => Op (Par a :-> Full a)
    ParNew    :: Type a => Op (Full (Par (IV a)))
    ParGet    :: Type a => Op (IV a :-> Full (Par a))
    ParPut    :: Type a => Op (IV a :-> a :-> Full (Par ()))
    ParFork   ::           Op (Par () :-> Full (Par ()))
    ParYield  ::           Op (Full (Par ()))

    -- | RealFloat
    Atan2   :: (Type a, RealFloat a) => Op (a :-> a :-> Full a)

    -- | Save
    Save :: Type a => Op (a :-> Full a)

    -- When are two size prop operators equal?
    -- | SizeProp
    PropSize :: (Type a, Type b) =>
        EqBox (Size a -> Size b) -> Op (a :-> b :-> Full b)

    -- | Switch
    Switch :: (Type b) => Op (b :-> Full b)

    -- Tuple
    Tup0  :: Op (Full ())
    Tup2  :: Op (a :-> b :-> Full (a,b))
    Tup3  :: Op (a :-> b :-> c :-> Full (a,b,c))
    Tup4  :: Op (a :-> b :-> c :-> d :-> Full (a,b,c,d))
    Tup5  :: Op (a :-> b :-> c :-> d :-> e :-> Full (a,b,c,d,e))
    Tup6  :: Op (a :-> b :-> c :-> d :-> e :-> f :-> Full (a,b,c,d,e,f))
    Tup7  :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :->
                 Full (a,b,c,d,e,f,g))
    Tup8  :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :->
                 Full (a,b,c,d,e,f,g,h))
    Tup9  :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :->
                 Full (a,b,c,d,e,f,g,h,i))
    Tup10 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 Full (a,b,c,d,e,f,g,h,i,j))
    Tup11 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :->
                 Full (a,b,c,d,e,f,g,h,i,j,k))
    Tup12 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :-> l :->
                 Full (a,b,c,d,e,f,g,h,i,j,k,l))
    Tup13 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :-> l :-> m :->
                 Full (a,b,c,d,e,f,g,h,i,j,k,l,m))
    Tup14 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :-> l :-> m :-> n :->
                 Full (a,b,c,d,e,f,g,h,i,j,k,l,m,n))
    Tup15 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :-> l :-> m :-> n :-> o :->
                 Full (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))

    Sel1  :: (Tuply w, Unpack w ~ (a,z))                     => Op (w :-> Full a)
    Sel2  :: (Tuply w, Unpack w ~ (a,(b,z)))                 => Op (w :-> Full b)
    Sel3  :: (Tuply w, Unpack w ~ (a,(b,(c,z))))             => Op (w :-> Full c)
    Sel4  :: (Tuply w, Unpack w ~ (a,(b,(c,(d,z)))))         => Op (w :-> Full d)
    Sel5  :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,z))))))     => Op (w :-> Full e)
    Sel6  :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,(f,z))))))) => Op (w :-> Full f)
    Sel7  :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,z)))))))
              ) => Op (w :-> Full g)
    Sel8  :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,z))))))))
              ) => Op (w :-> Full h)
    Sel9  :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,z)))))))))
              ) => Op (w :-> Full i)
    Sel10 :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,z))))))))))
              ) => Op (w :-> Full j)
    Sel11 :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,z)))))))))))
              ) => Op (w :-> Full k)
    Sel12 :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,z))))))))))))
              ) => Op (w :-> Full l)
    Sel13 :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,z)))))))))))))
              ) => Op (w :-> Full m)
    Sel14 :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,(n,z))))))))))))))
              ) => Op (w :-> Full n)
    Sel15 :: (Tuply w,
              Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,(n,(o,z)))))))))))))))
              ) => Op (w :-> Full o)

    -- | ConditionM
    ConditionM :: (Monad m, Type a) => Op (Bool :-> m a :-> m a :-> Full (m a))

    -- | LoopM
    While :: (Monad m, Size (m ()) ~ AnySize) => Op (m Bool :-> m a :-> Full (m ()))
    For   :: (Monad m, Size (m ()) ~ AnySize) => Op (Length :-> (Index -> m a) :-> Full (m ()))

    -- | Mutable
    Return :: Monad m => Op (a    :-> Full (m a))
    Bind   :: Monad m => Op (m a  :-> (a -> m b) :-> Full (m b))
    Then   :: Monad m => Op (m a  :-> m b        :-> Full (m b))
    When   :: Monad m => Op (Bool :-> m ()       :-> Full (m ()))


deriving instance Eq (Op a)
deriving instance Show (Op a)

-- | Utility functions

fvi :: AExpr a -> S.Set VarId
fvi (_ :& e) = fviR e

fviR :: Expr a -> S.Set VarId
fviR (Variable v) = viSet v
fviR (f :@ e) = fviR f `S.union` fvi e
fviR (Lambda v e) = fvi e S.\\ viSet v
fviR _ = S.empty

viSet :: Var a -> S.Set VarId
viSet v = S.singleton $ varNum v


data CBind where
  CBind :: (ExprCtx a) => Var a -> AExpr a -> CBind

instance Eq CBind where
  CBind (v1 :: Var a) e1 == CBind (v2 :: Var b) e2
      = case eqT :: Maybe (a :~: b) of
          Nothing -> False
          Just Refl -> varNum v1 == varNum v2 && e1 == e2

instance Show CBind where
  show (CBind v e) = show v ++ " = " ++ show e

bvId :: CBind -> VarId
bvId (CBind v _) = varNum v

fviB :: CBind -> S.Set VarId
fviB (CBind _ e) = fvi e

showRhs (CBind _ e) = show e

mkLets :: ExprCtx a => ([CBind], AExpr a) -> AExpr a
mkLets (CBind v rhs : bs, e) = aeInfo e :& Operator Let :@ rhs :@ (funInfo v e :& Lambda v (mkLets (bs,e)))
mkLets ([], e) = e

-- | Functions for bind environments
type BindEnv = M.Map VarId CBind

lookupBE :: Typeable a => String -> BindEnv -> Var a -> AExpr a
lookupBE msg bm (v :: Var a)
               = case M.lookup (varNum v) bm of
                      Nothing -> error $ msg ++ ": lookupBE does not find variable " ++ show v
                      Just (CBind (u :: Var b) e)
                           -> case eqT :: Maybe (a :~: b) of
                                   Nothing -> error $ msg ++ ": lookupBE finds conflicing types for " ++ show v
                                   Just Refl -> e

extendBE :: BindEnv -> CBind -> BindEnv
extendBE bm b = M.insert (bvId b) b bm

-- | Expressions that can and should be shared
sharable :: TypeF a => AExpr a -> Bool
sharable e = legalToShare e && goodToShare e

-- | Expressions that can be shared without breaking fromCore
legalToShare :: AExpr a -> Bool
legalToShare (_ :& Operator op) = shOp op
legalToShare (_ :& f :@ _)      = shApp f
legalToShare (_ :& Lambda _ _)  = False
legalToShare _                  = True

shApp :: Expr a -> Bool
shApp (f :@ _) = shApp f
shApp (Operator op) = shOp op

shOp :: Op a -> Bool
-- Elements
shOp ESkip     = False
shOp EWrite    = False
shOp EPar      = False
shOp EparFor   = False
-- Monads
shOp Return    = False
shOp Bind      = False
shOp Then      = False
shOp When      = False
-- Monadic arrays
shOp NewArr    = False
shOp NewArr_   = False
shOp GetArr    = False
shOp SetArr    = False
shOp ArrLength = False
-- Monadic loops
shOp For       = False
shOp While     = False
-- MonadRef
shOp NewRef    = False
shOp GetRef    = False
shOp SetRef    = False
shOp ModRef    = False
-- Everything else
shOp _ = True

-- | Expressions that are expensive enough to be worth sharing
goodToShare :: TypeF a => AExpr a -> Bool
goodToShare (_ :& Literal (l :: a)) = largeLit (typeRepF :: TypeRep a) l
goodToShare (_ :& _ :@ _) = True
goodToShare _                   = False

largeLit :: TypeRep a -> a -> Bool
largeLit UnitType l = False
largeLit BoolType l = False
largeLit (IntType _ _) l = False
largeLit FloatType l = False
largeLit DoubleType l = False
largeLit (ArrayType t) l = not $ null l
largeLit (ElementsType t) l = False
largeLit _ _ = True
