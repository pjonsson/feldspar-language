{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- Copyright (c) 2009-2011, ERICSSON AB
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

module Feldspar.Core.Constructs.ParFor where

import Control.Monad.Writer
import Data.Array.IArray
import Data.Array.MArray (freeze)
import Data.Array.Unsafe (unsafeFreeze)
import System.IO.Unsafe

import Language.Syntactic
import Language.Syntactic.Constructs.Monad
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Literal

import Data.Map (notMember)
import Data.Typeable (gcast)

data ParForFeat a
  where
    PParRun    :: Type a => ParForFeat (ParFor a :-> Full [a])
--    PParNew    :: Type a => ParForFeat (Full (ParFor (IV a)))
    PParPut    :: Type a => ParForFeat (Index :-> a :-> Full (ParFor a))
    PParComb   :: Type a => ParForFeat (ParFor a :-> ParFor a :-> Full (ParFor a))

instance Semantic ParForFeat
  where
    semantics PParRun    = Sem "runPar" runParF
    semantics PParPut    = Sem "put" (\i e -> do
                                                tell [(i, e)]
                                                return e)
    semantics PParComb   = Sem "||" (>>)

runParF :: ParFor a -> [a] -- Wrong, but passes type checker!
runParF e = map snd . execWriter . unParFor $ e

{-runMutableArrayEval :: forall a . Mut (MArr a) -> [a]
runMutableArrayEval m = unsafePerformIO $
                        do marr <- m
                           iarr <- unsafeFreeze marr
                           return (elems (iarr :: Array WordN a))
-}

instance Equality ParForFeat where equal = equalDefault; exprHash = exprHashDefault
instance Render   ParForFeat where renderArgs = renderArgsDefault
instance ToTree   ParForFeat
instance Eval     ParForFeat where evaluate = evaluateDefault
instance EvalBind ParForFeat where evalBindSym = evalBindSymDefault
instance Sharable ParForFeat

instance AlphaEq dom dom dom env => AlphaEq ParForFeat ParForFeat dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable (MONAD ParFor)

instance SizeProp ParForFeat
  where
    sizeProp PParRun   (WrapFull a :* Nil) = universal -- infoSize a
    sizeProp PParPut   _                   = universal
    sizeProp PParComb  _                   = universal

instance ( MONAD ParFor :<: dom
         , ParForFeat :<: dom
         , Optimize dom dom
         )
      => Optimize ParForFeat dom
  where
    constructFeatUnOpt opts PParRun args   = constructFeatUnOptDefault opts PParRun args
--    constructFeatUnOpt opts PParNew args   = constructFeatUnOptDefaultTyp opts (ParForType $ IVarType typeRep) PParNew args
    constructFeatUnOpt opts PParPut args   = constructFeatUnOptDefaultTyp opts (ParForType typeRep) PParPut args
    constructFeatUnOpt opts PParComb args  = constructFeatUnOptDefaultTyp opts (ParForType typeRep) PParComb args


monadProxy :: P ParFor
monadProxy = P

instance SizeProp (MONAD ParFor)
  where
    sizeProp Return (WrapFull a :* Nil)      = infoSize a
    sizeProp Bind   (_ :* WrapFull f :* Nil) = snd $ infoSize f
    sizeProp Then   (_ :* WrapFull b :* Nil) = infoSize b
    sizeProp When   _                        = AnySize

instance ( MONAD ParFor :<: dom
         , (Variable :|| Type) :<: dom
         , CLambda Type :<: dom
         , Let :<: dom
         , OptimizeSuper dom
         )
      => Optimize (MONAD ParFor) dom
  where
    optimizeFeat opts bnd@Bind (ma :* f :* Nil) = do
        ma' <- optimizeM opts ma
        case getInfo ma' of
          Info (ParForType ty) sz vs src -> do
            f' <- optimizeFunction opts (optimizeM opts) (Info ty sz vs src) f
            case getInfo f' of
              Info{} -> constructFeat opts bnd (ma' :* f' :* Nil)

    optimizeFeat opts a args = optimizeFeatDefault opts a args

    constructFeatOpt _ Bind (ma :* (lam :$ (ret :$ var)) :* Nil)
      | Just (SubConstr2 (Lambda v1)) <- prjLambda lam
      , Just Return                   <- prjMonad monadProxy ret
      , Just (C' (Variable v2))       <- prjF var
      , v1 == v2
      , Just ma' <- gcast ma
      = return ma'

    constructFeatOpt opts Bind (ma :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        , v `notMember` vars
        = constructFeat opts Then (ma :* body :* Nil)
      where
        vars = infoVars $ getInfo body

      -- return x >> mb ==> mb
    constructFeatOpt _ Then ((ret :$ _) :* mb :* Nil)
        | Just Return <- prjMonad monadProxy ret
        = return mb

      -- ma >> return () ==> ma
    constructFeatOpt _ Then (ma :* (ret :$ u) :* Nil)
        | Just Return <- prjMonad monadProxy ret
        , Just TypeEq <- typeEq (infoType $ getInfo ma)  (ParForType UnitType)
        , Just TypeEq <- typeEq (infoType $ getInfo ret) (ParForType UnitType)
        , Just ()     <- viewLiteral u
        = return ma

    constructFeatOpt opts a args = constructFeatUnOpt opts a args

    constructFeatUnOpt opts Return args@(a :* Nil)
        | Info {infoType = t} <- getInfo a
        = constructFeatUnOptDefaultTyp opts (ParForType t) Return args

    constructFeatUnOpt opts Bind args@(_ :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda _))  <- prjLambda lam
        , Info {infoType = t} <- getInfo body
        = constructFeatUnOptDefaultTyp opts t Bind args

    constructFeatUnOpt opts Then args@(_ :* mb :* Nil)
        | Info {infoType = t} <- getInfo mb
        = constructFeatUnOptDefaultTyp opts t Then args

    constructFeatUnOpt opts When args =
        constructFeatUnOptDefaultTyp opts voidTypeRep When args

