module Lambda.Pretty

import Lambda.Core
import Lambda.Church
import Data.SortedSet
import Data.String

freeNames : Expr -> SortedSet String
freeNames (BVar _) = empty
freeNames (FVar x) = singleton x
freeNames (Lam body) = freeNames body
freeNames (App f g) = (freeNames f) <+> (freeNames g)

candidate : Nat -> String
candidate i =
  let j : Int = cast i
      letter = cast (chr (ord 'a' + j `mod` 26))
      round = j `div` 26
  in if round == 0 then letter else letter ++ show round

lookupName : Nat -> List String -> Maybe String
lookupName _ [] = Nothing
lookupName Z (n :: _) = Just n
lookupName (S k) (_ :: ns) = lookupName k ns

export
pretty : Expr -> String
pretty e = go [] e
  where
    avoid : SortedSet String
    avoid = freeNames e

    fresh : List String -> String
    fresh scope = pick 0
      where
        pick : Nat -> String
        pick i =
          let c = candidate i
          in if contains c avoid || elem c scope then pick (S i) else c

    parens : String -> String
    parens s = "(" ++ s ++ ")"

    go : List String -> Expr -> String
    go scope (BVar k) = maybe ("^" ++ show k) id (lookupName k scope)
    go scope (FVar x) = x
    go scope (Lam body) = binders scope [] (Lam body)
      where
        binders : List String -> List String -> Expr -> String
        binders sc names (Lam b) = let n = fresh sc in binders (n :: sc) (names ++ [n]) b
        binders sc names b = "λ" ++ unwords names ++ ". " ++ go sc b
    go scope (App f g) = function f ++ " " ++ argument g
      where
        function : Expr -> String
        function l@(Lam _) = parens (go scope l)
        function other = go scope other

        argument : Expr -> String
        argument v@(BVar _) = go scope v
        argument v@(FVar _) = go scope v
        argument other = parens (go scope other)

export

traceReduction : Nat -> Expr -> IO ()
traceReduction fuel expr = do
  putStrLn ("0: " ++ pretty expr)
  go 0 fuel expr
  where
    showFinal : Nat -> Expr -> IO ()
    showFinal steps current =
      case Church.toNat 2 current of
        Just n =>
          putStrLn
            (show steps ++ ": " ++ pretty current ++ "\n" ++ "Result = " ++ show n)
        Nothing => do
          putStrLn (show steps ++ ": " ++ pretty current)
          putStrLn
            ("Normal form reached after "
              ++ show steps ++ " reductions.")

    go : Nat -> Nat -> Expr -> IO ()
    go steps remaining current =
      case reduce current of
        Nothing => showFinal steps current
        Just next =>
          case remaining of
            Z =>
              putStrLn
                ("Stopped: reduction limit reached after "
                  ++ show steps ++ " reductions.")

            S remaining' => do
              let nextStep = S steps

              case reduce next of
                Nothing => showFinal nextStep next

                Just _ => do
                  putStrLn
                    (show nextStep ++ ": " ++ pretty next)
                  go nextStep remaining' next
