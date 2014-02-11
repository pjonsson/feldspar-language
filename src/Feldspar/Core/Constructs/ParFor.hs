{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Data.List
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
    PParRun    :: Type a => ParForFeat (Length :-> ParFor a :-> Full [a])
    PParFor    :: Type a => ParForFeat (Length :-> (Index -> Index) :-> (Index -> ParFor a) :-> Full (ParFor a))
    PParRed    :: Type a => ParForFeat (Length :-> a :-> (Index -> a -> a) :-> Full (ParFor a))
    PParPut    :: Type a => ParForFeat (Index :-> a :-> Full (ParFor a))
    PParComb   :: Type a => ParForFeat (ParFor a :-> ParFor a :-> Full (ParFor a))

instance Semantic ParForFeat
  where
    semantics PParRun    = Sem "runPar" (\l (ParFor p) -> map snd p)
    semantics PParFor    = Sem "pFor" pParFor
    semantics PParRed    = Sem "pRed" pParRed
    semantics PParPut    = Sem "put" (\i e -> ParFor [(i, e)])
    semantics PParComb   = Sem "||" (\(ParFor l) (ParFor r) -> ParFor (l ++ r))


pParFor :: Length -> (Index -> Index) -> (Index -> ParFor a) -> ParFor a
pParFor len step ixf = ParFor $ concatMap (\(ParFor vs) -> vs) xs
      where xs = genericTake len $ map ixf $ iterate step 0

pParRed :: Length -> a -> (Index -> a -> a) -> ParFor a
pParRed len s0 ixf = undefined -- ParFor $ concatMap (\(ParFor vs) -> vs) xs
--      where xs = genericDrop 1 $ scanl (\st (ParFor [(_, e)]) -> ixf e st) s0 $ map (\i -> ParFor [(i,i)]) [1..(len+3)]


{-runMutableArrayEval :: forall a . Mut (MArr a) -> [a]
runMutableArrayEval m = unsafePerformIO $
                        do marr <- m
                           iarr <- unsafeFreeze marr
                           return (elems (iarr :: Array WordN a))
-}

semanticInstances ''ParForFeat

instance EvalBind ParForFeat where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq ParForFeat ParForFeat dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable ParForFeat

instance Typed ParForFeat
  where
    typeDictSym PParRun = Just Dict
    typeDictSym PParFor = Just Dict
    typeDictSym PParRed = Just Dict
    typeDictSym PParPut = Just Dict
    typeDictSym PParComb = Just Dict

instance SizeProp (ParForFeat :|| Type)
  where
    sizeProp (C' PParRun)   (WrapFull len :* WrapFull arr :* Nil) = infoSize arr
    sizeProp (C' PParFor)   _                   = universal
    sizeProp (C' PParRed)   _                   = universal
    sizeProp (C' PParPut)   _                   = universal
    sizeProp (C' PParComb)  (WrapFull p1 :* WrapFull p2 :* Nil) = universal -- TODO: p1 U p2

instance ( (ParForFeat :|| Type) :<: dom
         , OptimizeSuper dom
         )
      => Optimize (ParForFeat :|| Type) dom
  where
    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x
{-    constructFeatUnOpt opts PParRun args   = constructFeatUnOptDefault opts PParRun args
--    constructFeatUnOpt opts PParNew args   = constructFeatUnOptDefaultTyp opts (ParForType $ IVarType typeRep) PParNew args
    constructFeatUnOpt opts PParPut args   = constructFeatUnOptDefaultTyp opts (ParForType typeRep) PParPut args
    constructFeatUnOpt opts PParComb args  = constructFeatUnOptDefaultTyp opts (ParForType typeRep) PParComb args
-}
