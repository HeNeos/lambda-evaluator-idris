module Lambda.Combinator

import Lambda.Core

export
I : Expr
I = \. (^0)

export
K : Expr
K = \. \. (^1)

export
B : Expr
B = \. \. \. (^2 .$ (^1 .$ ^0))

export
C : Expr
C = \. \. \. ((^2 .$ ^0) .$ ^1)

export
W : Expr
W = \. \. ((^1 .$ ^0) .$ ^0)

export
U : Expr
U = \. (^0 .$ ^0)

export
S : Expr
S = \. \. \. ((^2 .$ ^0) .$ (^1 .$ ^0))

export
Y : Expr
Y = \. ((\. (^1 .$ (^0 .$ ^0))) .$ (\. (^1 .$ (^0 .$ ^0))))

export
Omega : Expr
Omega = (\. (^0 .$ ^0)) .$ (\. (^0 .$ ^0))
