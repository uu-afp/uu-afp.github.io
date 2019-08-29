module LambdaCalculus where

-- Lambda calculus terms with explicit names
type NamedTermVariable = String
data NamedTerm
  = NVar NamedTermVariable 
  | NApp NamedTerm NamedTerm
  | NLam NamedTermVariable NamedTerm
  | NNum Integer
  deriving (Show, Eq)

-- Examples
-- * \x -> x
nIdentity = NLam "x" (NVar "x")
-- * (\x -> x) 3
nExample = NApp nIdentity (NNum 3)

-- Lambda calculus using de Bruijn indices
type DeBruijnIndex = Integer
data DeBruijnTerm
  = BVar DeBruijnIndex
  | BApp DeBruijnTerm DeBruijnTerm
  | BLam DeBruijnTerm
  | BNum Integer
  deriving (Show, Eq)

-- Examples
-- * \x -> x
bIdentity = BLam (BVar 0)
-- * (\x -> x) 3
bExample = BApp bIdentity (BNum 3)

-- For each representation of lambda calculus,
-- where "Var" and "Term" refer to the concrete types:

-- * write an alpha-equivalence function
--      (\x. x) alpha-equiv to (\y. y) and so on
--      (\x y. x) is not alpha-equiv to (\x y. y)
--
-- alphaEquiv :: Term -> Term -> Bool

-- * write a substitution function
--
-- subst :: (Var, Term) -> Term -> Term
--
-- note: to ensure that variables are not accidentally captured
-- you have to implement a function to "freshen" all the
-- variables from lambdas
--
-- freshen :: Term -> Term

-- * write an evaluator which implements the beta-reduction rule
--      (\x. e) v --> subst x by v in e
-- 
-- eval :: Term -> Term