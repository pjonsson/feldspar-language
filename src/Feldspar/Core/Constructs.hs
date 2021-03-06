{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
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

module Feldspar.Core.Constructs where

import Data.Typeable
import Feldspar.Core.Syntactic

import Feldspar.Core.Types
import Feldspar.Core.Interpretation

--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------

newtype Data a = Data { unData :: ASTF a }

deriving instance Typeable Data

instance Syntactic (Data a)
  where
    type Internal (Data a) = a
    desugar = unData
    sugar   = Data

type SyntacticFeld a = (Syntactic a, TypeF (Internal a))

-- | Specialization of the 'Syntactic' class for first class values (eg not functions)
class    (SyntacticFeld a, Type (Internal a)) => Syntax a
instance (SyntacticFeld a, Type (Internal a)) => Syntax a
  -- It would be possible to let 'Syntax' be an alias instead of giving separate
  -- instances for all types. However, this leads to horrible error messages.
  -- For example, if 'Syntax' is an alias, the following expression gives a huge
  -- type error:
  --
  -- > eval (forLoop 10 0 (const (+id)))
  --
  -- The type error is not very readable now either, but at least it fits on the
  -- screen.

reifyF :: SyntacticFeld a => a -> ASTF (Internal a)
reifyF = desugar

instance Type a => Eq (Data a)
  where
    Data a == Data b = alphaEq (reifyF a) (reifyF b)

instance Type a => Show (Data a)
  where
    show = render . reifyF . unData
