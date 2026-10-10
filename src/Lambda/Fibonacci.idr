module Lambda.Fibonacci

import Lambda.Core
import Lambda.Operator
import Lambda.Church
import Lambda.Combinator

fibonacciFunctional : Expr
fibonacciFunctional = \. \. ((Operator.isZero .$ ^0 .$ Church.zero) .$ (((Operator.isZero .$ (Operator.pred .$ ^0)) .$ (Operator.succ .$ Church.zero)) .$ ((Operator.plus .$ (^1 .$ (Operator.pred .$ ^0))) .$ (^1 .$ (Operator.pred .$ (Operator.pred .$ ^0))))))

export
fibonacci : Expr
fibonacci = Combinator.Y .$ fibonacciFunctional
