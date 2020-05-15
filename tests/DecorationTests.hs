module Main where

-- To generate the golden files use a script similiar to this one
-- > ghc -isrc -iexampes -itests -e 'B.writeFile "tests/gold/example9.txt" $ B.pack $ showDecor example9' tests/DecorationTests.hs -iexamples

import qualified Prelude

import Test.Tasty
import Test.Tasty.Golden

import Data.ByteString.Lazy.UTF8 (fromString)

import Feldspar
import Feldspar.Mutable
import Examples.Simple.Basics
import Feldspar.Applications.TFModel(tfModel)

import Feldspar.Core.UntypedRepresentation (prettyExp)
import Feldspar.Core.Middleend.FromTyped (FrontendPass(FPUnAnnotate), frontend)
import Feldspar.Core.Middleend.PassManager (PassCtrl(..), defaultPassCtrl)



topLevelConsts :: Data Index -> Data Index -> Data Index
topLevelConsts a b = condition (b<5) (d ! (a+5)) (c ! (a+5))
  where
    c = value [1,2,3,4,5] :: Data [Index]
    d = value [2,3,4,5,6] :: Data [Index]

monadicSharing :: Data Index -> Data Index
monadicSharing a = runMutable $ do
    b  <- newRef a
    b' <- getRef b
    c  <- newRef (b'+3)
    c' <- getRef c
    return (c'+(b'+3))

-- An example with nested sharing. Our first codeMotion would miss the opportunity to share `(a+b)`.
trickySharing :: Data Index -> Data Index
trickySharing x = (a+b+c) + (a+b) + (a+b+c)
  where
    a = x*3
    b = x*5
    c = x*7

-- Compile an expression to untyped IL and show it as a string
showUntyped :: Syntactic a => FeldOpts -> a -> String
showUntyped opts = Prelude.head . Prelude.fst . frontend passCtrl opts . reifyFeld opts N32

-- Pass control to get IL after optimization
passCtrl :: PassCtrl FrontendPass
passCtrl = defaultPassCtrl{wrBefore = [FPUnAnnotate], stopBefore = [FPUnAnnotate]}

ref :: Prelude.String -> Prelude.String
ref f = "tests/gold/" Prelude.++ f

tests = testGroup "DecorationTests"
    [ goldenVsFile "example9" (ref "example9.txt") "tests/example9.txt" $ writeFile "tests/example9.txt" $ showDecor example9
    , goldenVsFile "topLevelConsts" (ref "topLevelConsts.txt") "tests/topLevelConsts.txt" $ writeFile "tests/topLevelConsts.txt" $ showDecor topLevelConsts
    , goldenVsFile "monadicSharing" (ref "monadicSharing.txt") "tests/monadicSharing.txt" $ writeFile "tests/monadicSharing.txt" $ showDecor monadicSharing
    , goldenVsFile "trickySharing" (ref "trickySharing.txt") "tests/trickySharing.txt" $ writeFile "tests/trickySharing.txt" $ showDecor trickySharing
    , goldenVsFile "tfModel" (ref "tfModel.txt") "tests/tfModel.txt" $ writeFile "tests/tfModel.txt" $ showUntyped defaultFeldOpts tfModel
    ]

main = defaultMain $ testGroup "Tests" [tests]

