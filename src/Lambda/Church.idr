module Lambda.Church

import Lambda.Core

-- Numerals

-- n = λf.λx.f (f ... (f x)), with f = #1, x = #0
export
fromNat : Nat -> Expr
fromNat n = \. \. Core.times n (^1 .$) (^0)

export
toNat : Nat -> Expr -> Maybe Nat
toNat fuel n = n .$ "f" .$ "x" |> Core.normalize fuel |> countApps
  where
    countApps : Expr -> Maybe Nat
    countApps (FVar "x") = Just 0
    countApps (App (FVar "f") rest) = S <$> countApps rest
    countApps _ = Nothing

-- Booleans

-- λt f. t
export
true : Expr
true = \. \. ^1

-- λt f. f
export
false : Expr
false = \. \. ^0

export
toBool : Nat -> Expr -> Maybe Bool
toBool fuel b =
  case normalize fuel (b .$ "t" .$ "f") of
    FVar "t" => Just True
    FVar "f" => Just False
    _ => Nothing


export
zero : Expr
zero = \. \. ^0
