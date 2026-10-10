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

--main =
--  Pretty.showResult 50000
--    (Factorial.factorial .$ Church.fromNat 5)

main =
  Pretty.showResult 1_000_000
    (Fibonacci.fibonacci .$ Church.fromNat 13)
