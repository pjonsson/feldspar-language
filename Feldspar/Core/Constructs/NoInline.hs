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

{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.Constructs.NoInline where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data NoInline a
  where
    NoInline :: (Type a) => NoInline (a :-> Full a)

instance WitnessCons NoInline
  where
    witnessCons NoInline = ConsWit

instance WitnessSat NoInline
  where
    type SatContext NoInline = TypeCtx
    witnessSat NoInline = SatWit

instance MaybeWitnessSat ctx NoInline
  where
    maybeWitnessSat _ _ = Nothing

instance Semantic NoInline
  where
    semantics NoInline  = Sem "NoInline" id

instance ExprEq   NoInline where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   NoInline where renderPart = renderPartSem
instance ToTree   NoInline
instance Eval     NoInline where evaluate = evaluateSem
instance EvalBind NoInline where evalBindSym = evalBindSymDefault
instance SizeProp NoInline where sizeProp = sizePropDefault
instance Sharable NoInline

instance AlphaEq dom dom dom env => AlphaEq NoInline NoInline dom env
  where
    alphaEqSym = alphaEqSymDefault

instance (NoInline :<: dom, Optimize dom dom) => Optimize NoInline dom
  where
    constructFeatUnOpt = constructFeatUnOptDefault
