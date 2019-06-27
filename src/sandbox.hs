
-- NOTE: Term could be a monad but I'd consider it really an abuse:
-- ~ substitute :: Unifier -> Variable -> Term
-- ~ substitute u v = fromMaybe (V v) (lookup v u)

-- ~ unify :: Predicate -> Predicate -> [Solution]
-- ~ unify p1 p2 = foldM comb identity $ zip p1 p2

--{x/a, y/f(z), z/y}yτ={u/a, x/b, y/z, z/g(x)}
sigma1 = [(Variable 0 "X", A (Atom "a")),
          (Variable 0 "Y", P (Predicate True "f" [V (Variable 0 "Z")])),
          (Variable 0 "Z", V (Variable 0 "Y"))]

sigma2 = [(Variable 0 "U", A (Atom "a")),
          (Variable 0 "X", A (Atom "b")),
          (Variable 0 "Y", V (Variable 0 "Z")),
          (Variable 0 "Z", P (Predicate True "g" [V (Variable 0 "X")]))]

{--
[(Variable 0 "X",A (Atom "a")),
(Variable 0 "Y", P (Predicate True "f" [P (Predicate True "g" [V (Variable 0 "X")])])),
(Variable 0 "U", A (Atom "a"))]
--}
