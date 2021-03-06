% Vector library

  <br/>



*[This document is just a port of [Simple vector library](SimpleVector.html), but it should be extended to cover the multi-dimensional aspects of the vector library.]*

The vector library provides an interface for multi-dimensional vectors. It is available in the module `Feldspar.Vector`:

\begin{code}
module Tutorial.Vector where

import qualified Prelude
import Feldspar
import Feldspar.Vector
\end{code}

Scalar product:

\begin{code}
scProd :: (Numeric a) => Pull1 a -> Pull1 a -> Data a
scProd a b = fromZero $ sum (zipWith (*) a b)
\end{code}

Specialize the type:

\begin{code}
scProdF = scProd :: Pull1 Float -> Pull1 Float -> Data Float
\end{code}

Testing:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Tutorial.Vector> eval scProdF [1,2,3,4] [5,6,7,8::Float]
70.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Resulting core expression (with manually inserted white space):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Tutorial.Vector> printExpr scProdF
(\var0 -> (\var1 -> (
    forLoop (min (getLength var0) (getLength var1)) 0.0 (\var2 -> (\var3 ->
        (var3 + ((var0 ! var2) * (var1 ! var2)))
    ))
)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note how `sum` and `zipWith` have been fused into a single `forLoop`.

