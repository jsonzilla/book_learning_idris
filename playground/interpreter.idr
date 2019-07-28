module interpreter

import Data.Vect

data Ty = TyInt | TyBool | TyFun Ty Ty

total interpTy : Ty -> Type
interpTy TyInt = Int
interpTy TyBool = Bool
interpTy (TyFun a t) = interpTy a -> interpTy t

using (G:Vect n ty)

data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
  -- Stop : HasType FZ (t :: G) t
  Pop : HasType k G t -> HasType (FS k) (u :: G) t


-- data Expr : Vect n Ty -> Ty -> Type where
--   Var : HasType i G t -> Expr G t
--   Val : (x : Int) -> Expr G TyInt
--   Lam : Expr (a :: G) t -> Expr G (TyFun a t)
--   App : Expr G (TyFun a t) -> Expr G a -> Expr G t
--   Op : (interpTy a -> interpTy b -> interpTy c) -> Expr G a ->
--         Expr G b -> Expr G c
--   If : Expr G TyBool -> Lazy (Expr G a) -> Lazy (Expr G a) -> Expr G a
