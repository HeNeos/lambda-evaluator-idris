module Main

import Data.SortedSet
import Data.Nat

pred : (n: Nat) -> {auto 0 _ : NonZero n} -> Nat
pred (S k) = k

data Expr =
  BVar Nat
  | FVar String
  | Lam Expr
  | App Expr Expr

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

substTop : Expr -> Expr -> Expr
substTop value body =
  shiftDown 0 (subst 0 (shiftUp 1 0 value) body)

-- Reduce
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
normalize : Nat -> Expr -> Expr
normalize Z expr = expr
normalize (S fuel) expr =
  case reduce expr of
    Just expr' => normalize fuel expr'
    Nothing => expr

identity : Expr
identity = Lam (BVar 0)

-- Test

example : Expr
example = App identity (FVar "y")

main : IO ()
main = printLn (normalize 100 example)
