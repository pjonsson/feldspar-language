{-# LANGUAGE FlexibleContexts #-}
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

module Feldspar.ParFor
  ( runPPar
  , pFor
  , pRed
  , putP
  , combP
  )
where

import Feldspar.Core.Constructs (sugarSymF)
import Feldspar.Core.Constructs.ParFor
import Feldspar.Core.Frontend.ParFor

import Feldspar

runPPar :: Type a => Data Length -> Data (ParFor a) -> Data [a]
runPPar = sugarSymF PParRun

pFor :: Type a => Data Length -> (Data Index -> Data Index) -> (Data Index -> Data (ParFor a)) -> Data (ParFor a)
pFor = sugarSymF PParFor

pRed :: Type a => Data Length -> Data a -> (Data Index -> Data a -> Data a) -> Data (ParFor a)
pRed = sugarSymF PParRed

putP :: Type a => Data Index -> Data a -> Data (ParFor a)
putP = sugarSymF PParPut

combP :: Type a => Data (ParFor a) -> Data (ParFor a) -> Data (ParFor a)
combP = sugarSymF PParComb

