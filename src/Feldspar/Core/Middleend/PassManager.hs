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

module Feldspar.Core.Middleend.PassManager where

data Prog a b = Prog (Maybe a) [String] b
  deriving Show

data PassCtrl a = PassCtrl
       { wrBefore   :: [a]
       , wrAfter    :: [a]
       , stopBefore :: [a]
       , stopAfter  :: [a]
       , skip       :: [a]
       }
  deriving Show

addWrBefore :: PassCtrl a -> a -> PassCtrl a
addWrBefore ctrl p = ctrl{wrBefore = p : wrBefore ctrl}

addWrAfter :: PassCtrl a -> a -> PassCtrl a
addWrAfter ctrl p = ctrl{wrAfter = p : wrAfter ctrl}

setStopBefore :: PassCtrl a -> a -> PassCtrl a
setStopBefore ctrl p = ctrl{stopBefore = [p]}

setStopAfter :: PassCtrl a -> a -> PassCtrl a
setStopAfter ctrl p = ctrl{stopAfter = [p]}

addSkip :: PassCtrl a -> a -> PassCtrl a
addSkip ctrl p = ctrl{skip = p : skip ctrl}

defaultPassCtrl :: PassCtrl a
defaultPassCtrl = PassCtrl [] [] [] [] []

class Pretty a where
  pretty :: a -> String

prOrStop :: (Pretty a, Eq b, Show b) => String -> [b] -> [b] -> b -> Prog a c -> Prog a c
prOrStop pos prs stop pass (Prog (Just p) ss s)
  = Prog (if pass `elem` stop then Nothing else Just p)
         (ss ++ [preamble ++ pretty p ++ "\n" | pass `elem` prs])
         s
  where preamble = "\n========== " ++ pos ++ " " ++ show pass ++ " ==========\n\n"
prOrStop _ _ _ _ prog = prog

runPassC :: Eq b => [b] -> b -> (a -> a) -> Prog a c -> Prog a c
runPassC skips pass f (Prog (Just p) ss s)
  = Prog (Just $ if pass `elem` skips then p else f p) ss s
runPassC _ _ _ prog = prog

runPassT :: (a -> b) -> Prog a c -> Prog b c
runPassT f (Prog (Just p) ss s) = Prog (Just $ f p) ss s
runPassT f (Prog Nothing ss s) = Prog Nothing ss s

runPassS :: Eq b => [b] -> b -> ((c,a) -> (c,a)) -> Prog a c -> Prog a c
runPassS skips pass f (Prog (Just p) ss s)
  | pass `notElem` skips = Prog (Just p1) ss s1
  where (s1,p1) = f (s,p)

passC :: (Pretty a, Eq b, Show b) => PassCtrl b -> b -> (a -> a) -> Prog a c -> Prog a c
passC ctrl pass f = prOrStop "After" (wrAfter ctrl) (stopAfter ctrl) pass
                  . runPassC (skip ctrl) pass f
                  . prOrStop "Before" (wrBefore ctrl) (stopBefore ctrl) pass

passT :: (Pretty a, Pretty d, Eq b, Show b) => PassCtrl b -> b -> (a -> d) -> Prog a c -> Prog d c
passT ctrl pass f = prOrStop "After" (wrAfter ctrl) (stopAfter ctrl) pass
                  . runPassT f
                  . prOrStop "Before" (wrBefore ctrl) (stopBefore ctrl) pass

passS :: (Pretty a, Eq b, Show b) => PassCtrl b -> b -> ((c,a) -> (c,a)) -> Prog a c -> Prog a c
passS ctrl pass f = prOrStop "After" (wrAfter ctrl) (stopAfter ctrl) pass
                  . runPassS (skip ctrl) pass f
                  . prOrStop "Before" (wrBefore ctrl) (stopBefore ctrl) pass

evalPasses :: c -> (Prog a c -> Prog b c) -> a -> ([String], Maybe b)
evalPasses s f p = case f $ Prog (Just p) [] s of
                     Prog prg ss _ -> (ss, prg)

