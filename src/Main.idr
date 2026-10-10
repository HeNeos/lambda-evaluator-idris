module Main

import Lambda.Core
import Lambda.Church
import Lambda.Pretty
import Lambda.Fibonacci
import Lambda.Factorial

main : IO ()

--main =
--  traceReduction 1000
--    (fibonacci .$ Church.fromNat 4)

main = traceReduction 1000 (factorial .$ Church.fromNat 3)
