module Lambda.Factorial

import Lambda.Core
import Lambda.Operator
import Lambda.Church
import Lambda.Combinator

factorialFunctional : Expr
factorialFunctional = \. \. ((Operator.isZero .$ ^0 .$ (Operator.succ .$ Church.zero)) .$ ((Combinator.B .$ ^0) .$ (^1 .$ (Operator.pred .$ ^0))))

export
factorial : Expr
factorial = Combinator.Y .$ factorialFunctional
