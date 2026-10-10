module Lambda.Core

import Data.SortedSet
import Data.Nat


public export
data Expr =
  BVar Nat
  | FVar String
  | Lam Expr
  | App Expr Expr

export
Show Expr where
  show (BVar x) = "#" ++ show x
  show (FVar x) = x
  show (Lam body) = "(λ." ++ show body ++ ")"
  show (App f g) = "(" ++ show f ++ " " ++ show g ++ ")"

shiftUp : Nat -> Nat -> Expr -> Expr
shiftUp amount level (BVar k) =
  if k < level
     then BVar k
     else BVar (k + amount)

shiftUp _ _ (FVar k) = FVar k
shiftUp amount level (Lam body) = Lam (shiftUp amount (S level) body)
shiftUp amount level (App f g) = App (shiftUp amount level f) (shiftUp amount level g)

shiftDown : Nat -> Expr -> Expr
shiftDown level (BVar k) =
  if k < level
    then BVar k
    else BVar (pred k)

shiftDown _ (FVar x) = FVar x
shiftDown level (Lam body) = Lam (shiftDown (S level) body)
shiftDown level (App f arg) = App (shiftDown level f) (shiftDown level arg)

-- Substitution
subst : Nat -> Expr -> Expr -> Expr
subst target varValue (BVar k) = if k == target then varValue else BVar k
subst _ _ (FVar k) = FVar k
subst target varValue (Lam body) = Lam (subst (S target) (shiftUp 1 0 varValue) body)
subst target varValue (App f g) =
  App (subst target varValue f) (subst target varValue g)

export
substTop : Expr -> Expr -> Expr
substTop value body =
  shiftDown 0 (subst 0 (shiftUp 1 0 value) body)

-- Reduce
export
reduce : Expr -> Maybe Expr
reduce (BVar _) = Nothing
reduce (FVar _) = Nothing

reduce (Lam body) =
  case reduce body of
    Just body' => Just (Lam body')
    Nothing => Nothing

reduce (App (Lam body) g) =
  Just (substTop g body)

reduce (App f g) =
  case reduce f of
    Just f' => Just (App f' g)
    Nothing =>
      case reduce g of
        Just g' => Just (App f g')
        Nothing => Nothing

-- Normalize
export
normalize : Nat -> Expr -> Expr
normalize Z expr = expr
normalize (S fuel) expr =
  case reduce expr of
    Just expr' => normalize fuel expr'
    Nothing => expr



-- Notation
-- \. body    abstraction; the body extends as far right as possible
-- f .$ x     application; left-associative, so f .$ x .$ y = (f x) y
-- ^n         bound variable pointing n binders up (de Bruijn index n)
-- "x"        free variable
-- e.g. λ.λ.1 (1 0) is written \. \. (^1) .$ (^1 .$ ^0)

export prefix 10 ^
export infixl 9 .$
export prefix 1 \.

export
(^) : Nat -> Expr
(^) = BVar

export
(.$) : Expr -> Expr -> Expr
(.$) = App

export
(\.) : Expr -> Expr
(\.) = Lam

export
FromString Expr where
  fromString = FVar

export
times : Nat -> (a -> a) -> a -> a
times Z _ x = x
times (S k) f x = f (times k f x)
