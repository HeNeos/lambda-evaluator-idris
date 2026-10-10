module Lambda.Operator

import Lambda.Core
import Lambda.Church

-- λn f x. f (n f x) 
export
succ : Expr
succ = \. \. \. (^1 .$ (^2 .$ ^1 .$ ^0))

-- λm n f x. m f (n f x)
export
plus : Expr
plus = \. \. \. \. (^3 .$ ^1 .$ (^2 .$ ^1 .$ ^0))

-- λm n f. m (n f)
export
mult : Expr
mult = \. \. \. (^2 .$ (^1 .$ ^0))

-- λm n. n m
export
pow : Expr
pow = \. \. (^0 .$ ^1)

-- λn f x. n (λg h. h (g f)) (λu. x) (λu. u)
export
pred : Expr
pred = \. \. \. (^2 .$ (\. \. (^0 .$ (^1 .$ ^3))) .$ (\. ^1) .$ (\. ^0))

-- λm n. n pred m
export
sub : Expr
sub = \. \. (^0 .$ pred .$ ^1)

-- λn. n (λ_. false) true
export
isZero : Expr
isZero = \. ^0 .$ (\. Church.false) .$ Church.true

export
pair : Expr
pair = \. \. \. (^0 .$ ^2 .$ ^1)

export
first : Expr
first = \. (^0 .$ Church.true)

export
second : Expr
second = \. (^0 .$ Church.false)

export
slide : Expr
slide = \. ((pair .$ (second .$ ^0)) .$ (succ .$ (second .$ ^0)))
