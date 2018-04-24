{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module is the poly kinded version of Data.Row.Internal
module Data.Row.Poly
  -- * Rows
  ( Row(..)
  , Label(..)
  , KnownSymbol
  , LT(..)
  , Empty
  , HideType(..)
  -- * Row Operations
  , Extend
  , Modify
  , Rename
  , type (.\)
  , type (.!)
  , type (.-)
  , type (.+)
  , type (.\\)
  , type (.==)
  , type (.\/)
  , Lacks
  , HasType
  -- * Row Classes
  , Labels, labels, labels'
  , Forall(..), Forall2(..)
  , Unconstrained1
  -- * Helper functions
  , show'
  , toKey
  , type (≈)
  , mapForall
  , freeForall
  , uniqueMap
  , IsA(..)
  , As(..)
  , AllUniqueLabels
  , Disjoint
  , Zip
  , Map
  , Subset
  , Ifte
  ) where

import Data.Constraint
import Data.Functor.Const
import Data.Proxy
import Data.Row.Internal
       (type (≈), Empty, HideType(..), LT(..), Label(..), Labels, Row(..), As(..), IsA(..),
        Unconstrained1, show', toKey)
import Data.String (IsString(fromString))
import Data.Type.Equality (type (==))
import GHC.Types (Type)
import GHC.TypeLits
import Prelude
import qualified Unsafe.Coerce as UNSAFE

{--------------------------------------------------------------------
  Row operations
--------------------------------------------------------------------}
infixl 4 .\ {- This comment needed to appease CPP -}

-- | Does the row lack (i.e. it does not have) the specified label?
type family (r :: Row k) .\ (l :: Symbol) :: Constraint where
  R r .\ l = LacksR l r r

-- | Type level Row extension
type family Extend (l :: Symbol) (a :: k) (r :: Row k) :: Row k where
  Extend l a (R x) = R (Inject (l :-> a) x)

-- | Type level Row modification
type family Modify (l :: Symbol) (a :: k) (r :: Row k) :: Row k where
  Modify l a (R ρ) = R (ModifyR l a ρ)

-- | Type level row renaming
type family Rename (l :: Symbol) (l' :: Symbol) (r :: Row k) :: Row k where
  Rename l l' r = Extend l' (r .! l) (r .- l)

infixl 5 .!

-- | Type level label fetching
type family (r :: Row k) .! (t :: Symbol) :: k where
  R r .! l = Get l r

infixl 6 .-

-- | Type level Row element removal
type family (r :: Row k) .- (s :: Symbol) :: Row k where
  R r .- l = R (Remove l r)

infixl 6 .+

-- | Type level Row append
type family (l :: Row k) .+ (r :: Row k) :: Row k where
  R l .+ R r = R (Merge l r)

infixl 6 .\\ {- This comment needed to appease CPP -}

-- | Type level Row difference.  That is, @l .\\\\ r@ is the row remaining after
-- removing any matching elements of @r@ from @l@.
type family (l :: Row k) .\\ (r :: Row k) :: Row k where
  R l .\\ R r = R (Diff l r)

infixl 6 .\/

-- | The minimum join of the two rows.
type family (l :: Row k) .\/ (r :: Row k) :: Row k where
  R l .\/ R r = R (MinJoinR l r)

--
{--------------------------------------------------------------------
  Syntactic sugar for record operations
--------------------------------------------------------------------}
-- | Alias for '.\'. It is a class rather than an alias, so that
-- it can be partially applied.
class Lacks (l :: Symbol) (r :: Row k)

instance (r .\ l) => Lacks l r

-- | Alias for @(r .! l) ≈ a@. It is a class rather than an alias, so that
-- it can be partially applied.
class (r .! l ≈ a) =>
      HasType l a r

instance (r .! l ≈ a) => HasType l a r

-- | A type level way to create a singleton Row.
infix 7 .==

type (l :: Symbol) .== (a :: k) = Extend l a Empty


{--------------------------------------------------------------------
  Constrained record operations
--------------------------------------------------------------------}

-- | Proof that the given label is a valid candidate for the next step
-- in a metamorph fold, i.e. it's not in the list yet and, when sorted,
-- will be placed at the head.
type FoldStep ℓ τ ρ = Inject (ℓ :-> τ) ρ ≈ ℓ :-> τ ': ρ

-- | Any structure over a row in which every element is similarly constrained can
--   be metamorphized into another structure over the same row.
class Forall (r :: Row k) (c :: k -> Constraint) where
  -- | A metamorphism is an unfold followed by a fold.  This one is for
  -- product-like row-types (e.g. Rec).
  metamorph :: forall (f :: Row k -> Type) (g :: Row k -> Type) (h :: k -> Type).
               Proxy h
            -> (f Empty -> g Empty)
               -- ^ The way to transform the empty element
            -> (forall ℓ τ ρ. (KnownSymbol ℓ, c τ) => Label ℓ -> f ('R (ℓ :-> τ ': ρ)) -> (h τ, f ('R ρ)))
               -- ^ The unfold
            -> (forall ℓ τ ρ. (KnownSymbol ℓ, c τ, FoldStep ℓ τ ρ) => Label ℓ -> h τ -> g ('R ρ) -> g ('R (ℓ :-> τ ': ρ)))
               -- ^ The fold
            -> f r  -- ^ The input structure
            -> g r

  -- | A metamorphism is an unfold followed by a fold.  This one is for
  -- sum-like row-types (e.g. Var).
  metamorph' :: forall (f :: Row k -> Type) (g :: Row k -> Type) (h :: k -> Type).
               Proxy h
            -> (f Empty -> g Empty)
               -- ^ The way to transform the empty element
            -> (forall ℓ τ ρ. (KnownSymbol ℓ, c τ) => Label ℓ -> f ('R (ℓ :-> τ ': ρ)) -> Either (h τ) (f ('R ρ)))
               -- ^ The unfold
            -> (forall ℓ τ ρ. (KnownSymbol ℓ, c τ, FoldStep ℓ τ ρ) => Label ℓ -> Either (h τ) (g ('R ρ)) -> g ('R (ℓ :-> τ ': ρ)))
               -- ^ The fold
            -> f r  -- ^ The input structure
            -> g r


-- | An internal type used by the 'metamorph' in 'mapForall'.
newtype MapForall (c :: Type -> Constraint) f (r :: Row Type) = MapForall { unMapForall :: Dict (Forall (Map f r) (IsA c f)) }


-- | This allows us to derive a `Forall (Map f r) ..` from a `Forall r ..`.
mapForall :: forall (f :: Type -> Type) (c :: Type -> Constraint) (ρ :: Row Type). Forall ρ c :- Forall (Map f ρ) (IsA c f)
mapForall = Sub $ unMapForall $ metamorph @Type @ρ @c @(Const ()) @(MapForall c f) @(Const ()) Proxy empty uncons cons $ Const ()
  where empty :: Const () Empty -> MapForall c f Empty
        empty _ = MapForall Dict

        uncons :: forall l t r. (KnownSymbol l, c t)
               => Label l -> Const () ('R (l :-> t ': r)) -> (Const () t, Const () ('R r))
        uncons _ _ = (Const (), Const ())

        cons :: forall l t r. (KnownSymbol l, c t, FoldStep l t r)
             => Label l -> Const () t -> MapForall c f ('R r)
             -> MapForall c f ('R (l :-> t ': r))
        cons _ _ (MapForall Dict) =
           case UNSAFE.unsafeCoerce @(Dict Unconstrained) @(Dict (FoldStep l (f t) (MapR f r))) Dict of
             Dict -> MapForall Dict

-- | Map preserves uniqueness of labels.
uniqueMap :: forall f ρ. AllUniqueLabels ρ :- AllUniqueLabels (Map f ρ)
uniqueMap = Sub $ UNSAFE.unsafeCoerce @(Dict Unconstrained) Dict

-- | Allow any 'Forall` over a row-type, be usable for 'Unconstrained1'.
freeForall :: forall r c. Forall r c :- Forall r Unconstrained1
freeForall = Sub $ UNSAFE.unsafeCoerce @(Dict (Forall r c)) Dict

instance Forall (R '[]) c where
  {-# INLINE metamorph #-}
  metamorph _ empty _ _ = empty
  {-# INLINE metamorph' #-}
  metamorph' _ empty _ _ = empty


instance (KnownSymbol ℓ, c τ, FoldStep ℓ τ ρ, Forall ('R ρ) c) => Forall ('R (ℓ :-> τ ': ρ) :: Row k) c where
  metamorph :: forall (f :: Row k -> Type) (g :: Row k -> Type) (h :: k -> Type).
               Proxy h
            -> (f Empty -> g Empty)
               -- ^ The way to transform the empty element
            -> (forall l t r. (KnownSymbol l, c t) => Label l -> f ('R (l :-> t ': r)) -> (h t, f ('R r)))
               -- ^ The unfold
            -> (forall l t r. (KnownSymbol l, c t, FoldStep l t r) => Label l -> h t -> g ('R r) -> g ('R (l :-> t ': r)))
               -- ^ The fold
            -> f ('R (ℓ :-> τ ': ρ))  -- ^ The input structure
            -> g ('R (ℓ :-> τ ': ρ))
  {-# INLINE metamorph #-}
  metamorph _ empty uncons cons r = cons Label t $ metamorph @k @('R ρ) @c @_ @_ @h Proxy empty uncons cons r'
    where (t, r') = uncons Label r
  metamorph' :: forall (f :: Row k -> Type) (g :: Row k -> Type) (h :: k -> Type).
               Proxy h
            -> (f Empty -> g Empty)
               -- ^ The way to transform the empty element
            -> (forall l t r. (KnownSymbol l, c t) => Label l -> f ('R (l :-> t ': r)) -> Either (h t) (f ('R r)))
               -- ^ The unfold
            -> (forall l t r. (KnownSymbol l, c t, FoldStep l t r) => Label l -> Either (h t) (g ('R r)) -> g ('R (l :-> t ': r)))
               -- ^ The fold
            -> f ('R (ℓ :-> τ ': ρ))  -- ^ The input structure
            -> g ('R (ℓ :-> τ ': ρ))
  {-# INLINE metamorph' #-}
  metamorph' _ empty uncons cons r = cons Label $ metamorph' @k @('R ρ) @c @_ @_ @h Proxy empty uncons cons <$> uncons Label r


-- | Any structure over two rows in which every element of both rows satisfies the
--   given constraint can be metamorphized into another structure over both of the
--   rows.

class Forall2 (r1 :: Row k) (r2 :: Row k) (c :: k -> Constraint) where
  -- | A metamorphism is a fold followed by an unfold.  Here, we fold both of the inputs.
  metamorph2 :: forall (f :: Row k -> Type) (g :: Row k -> Type) (h :: Row k -> Row k -> Type)
                       (f' :: k -> Type) (g' :: k -> Type).
                Proxy f' -> Proxy g'
             -> (f Empty -> g Empty -> h Empty Empty)
             -> (forall ℓ τ1 τ2 ρ1 ρ2. (KnownSymbol ℓ, c τ1, c τ2)
                 => Label ℓ
                 -> f ('R (ℓ :-> τ1 ': ρ1))
                 -> g ('R (ℓ :-> τ2 ': ρ2))
                 -> ((f' τ1, f ('R ρ1)), (g' τ2, g ('R ρ2))))
             -> (forall ℓ τ1 τ2 ρ1 ρ2. (KnownSymbol ℓ, c τ1, c τ2)
                 => Label ℓ -> f' τ1 -> g' τ2 -> h ('R ρ1) ('R ρ2) -> h ('R (ℓ :-> τ1 ': ρ1)) ('R (ℓ :-> τ2 ': ρ2)))
             -> f r1 -> g r2 -> h r1 r2

instance Forall2 (R '[]) (R '[]) c where
  {-# INLINE metamorph2 #-}
  metamorph2 _ _ empty _ _ = empty

instance (KnownSymbol ℓ, c τ1, c τ2, Forall2 ('R ρ1) ('R ρ2) c)
      => Forall2 ('R (ℓ :-> τ1 ': ρ1) :: Row k) ('R (ℓ :-> τ2 ': ρ2) :: Row k) c where
  {-# INLINE metamorph2 #-}
  metamorph2 f g empty uncons cons r1 r2 = cons (Label @ℓ) t1 t2 $ metamorph2 @k @('R ρ1) @('R ρ2) @c f g empty uncons cons r1' r2'
    where ((t1, r1'), (t2, r2')) = uncons (Label @ℓ) r1 r2

-- | A null constraint
class Unconstrained

instance Unconstrained

-- | Return a list of the labels in a row type.

labels :: forall ρ c s. (IsString s, Forall ρ c) => [s]
labels = getConst $ metamorph @_ @ρ @c @(Const ()) @(Const [s]) @(Const ()) Proxy (const $ Const []) doUncons doCons (Const ())
  where doUncons _ _ = (Const (), Const ())
        doCons l _ (Const c) = Const $ show' l : c

-- | Return a list of the labels in a row type and is specialized to the 'Unconstrained1' constraint.
labels' :: forall ρ s. (IsString s, Forall ρ Unconstrained1) => [s]
labels' = labels @ρ @Unconstrained1

{--------------------------------------------------------------------
  Convenient type families and classes
--------------------------------------------------------------------}
-- | A convenient way to provide common, easy constraints

type WellBehaved ρ = (Forall ρ Unconstrained1, AllUniqueLabels ρ)

-- | Are all of the labels in this Row unique?
type family AllUniqueLabels (r :: Row k) :: Constraint where
  AllUniqueLabels (R r) = AllUniqueLabelsR r

type family AllUniqueLabelsR (r :: [LT k]) :: Constraint where
  AllUniqueLabelsR '[] = Unconstrained
  AllUniqueLabelsR '[ l :-> a] = Unconstrained
  AllUniqueLabelsR (l :-> a ': l :-> b ': _) = TypeError (Text "The label " :<>: ShowType l :<>: Text " is not unique." :$$: Text "It is assigned to both " :<>: ShowType a :<>: Text " and " :<>: ShowType b)
  AllUniqueLabelsR (l :-> a ': l' :-> b ': r) = AllUniqueLabelsR (l' :-> b ': r)

-- | Is the first row a subset of the second?
type family Subset (r1 :: Row k) (r2 :: Row k) :: Constraint where
  Subset (R r1) (R r2) = SubsetR r1 r2

type family SubsetR (r1 :: [LT k]) (r2 :: [LT k]) :: Constraint where
  SubsetR '[] _ = Unconstrained
  SubsetR x '[] = TypeError (Text "One row-type is not a subset of the other." :$$: Text "The first contains the bindings " :<>: ShowType x :<>: Text " while the second does not.")
  SubsetR (l :-> a ': x) (l :-> a ': y) = SubsetR x y
  SubsetR (l :-> a ': x) (l :-> b ': y) = TypeError (Text "One row-type is not a subset of the other." :$$: Text "The first assigns the label " :<>: ShowType l :<>: Text " to " :<>: ShowType a :<>: Text " while the second assigns it to " :<>: ShowType b)
  SubsetR (hl :-> al ': tl) (hr :-> ar ': tr) = Ifte (hl <=.? hr) (TypeError (Text "One row-type is not a subset of the other." :$$: Text "The first assigns the label " :<>: ShowType hl :<>: Text " to " :<>: ShowType al :<>: Text " while the second has no assignment for it.")) (SubsetR (hl :-> al ': tl) tr)

-- | A type synonym for disjointness.

type Disjoint l r = ( WellBehaved l
                    , WellBehaved r
                    , Subset l (l .+ r)
                    , Subset r (l .+ r)
                    , l .+ r .\\ l ≈ r
                    , l .+ r .\\ r ≈ l)

-- | Map a type level function over a Row.
type family Map (f :: a -> b) (r :: Row a) :: Row b where
  Map f (R r) = R (MapR f r)

type family MapR (f :: a -> b) (r :: [LT a]) :: [LT b] where
  MapR f '[] = '[]
  MapR f (l :-> v ': t) = l :-> f v ': MapR f t

-- | Zips two rows together to create a Row of the pairs.
--   The two rows must have the same set of labels.
type family Zip (r1 :: Row a) (r2 :: Row b) :: Row (a, b) where
  Zip (R r1) (R r2) = R (ZipR r1 r2)

type family ZipR (r1 :: [LT a]) (r2 :: [LT b]) :: [LT (a, b)] where
  ZipR '[] '[] = '[]
  ZipR (l :-> t1 ': r1) (l :-> t2 ': r2) = l :-> '( t1, t2) ': ZipR r1 r2
  ZipR (l :-> t1 ': r1) _ = TypeError (Text "Row types with different label sets cannot be zipped" :$$: Text "For one, the label " :<>: ShowType l :<>: Text " is not in both lists.")
  ZipR '[] (l :-> t ': r) = TypeError (Text "Row types with different label sets cannot be zipped" :$$: Text "For one, the label " :<>: ShowType l :<>: Text " is not in both lists.")

type family Inject (l :: LT k) (r :: [LT k]) :: [LT k] where
  Inject (l :-> t) '[] = (l :-> t ': '[])
  Inject (l :-> t) (l :-> t' ': x) = TypeError (Text "Cannot inject a label into a row type that already has that label" :$$: Text "The label " :<>: ShowType l :<>: Text " was already assigned the type " :<>: ShowType t' :<>: Text " and is now trying to be assigned the type " :<>: ShowType t :<>: Text ".")
  Inject (l :-> t) (l' :-> t' ': x) = Ifte (l <=.? l') (l :-> t ': l' :-> t' ': x) (l' :-> t' ': Inject (l :-> t) x)

-- | Type level Row modification helper
type family ModifyR (l :: Symbol) (a :: k) (ρ :: [LT k]) :: [LT k] where
  ModifyR l a (l :-> a' ': ρ) = l :-> a ': ρ
  ModifyR l a (l' :-> a' ': ρ) = l' :-> a' ': ModifyR l a ρ
  ModifyR l a '[] = TypeError (Text "Tried to modify the label " :<>: ShowType l :<>: Text ", but it does not appear in the row-type.")

type family Get (l :: Symbol) (r :: [LT k]) :: k where
  Get l '[] = TypeError (Text "No such field: " :<>: ShowType l)
  Get l (l :-> t ': x) = t
  Get l (l' :-> t ': x) = Get l x

type family Remove (l :: Symbol) (r :: [LT k]) :: [LT k] where
  Remove l r = RemoveT l r r

type family RemoveT (l :: Symbol) (r :: [LT k]) (r_orig :: [LT k]) :: [LT k] where
  RemoveT l (l :-> t ': x) _ = x
  RemoveT l (l' :-> t ': x) r = l' :-> t ': RemoveT l x r
  RemoveT l '[] r = TypeError (Text "Cannot remove a label that does not occur in the row type." :$$: Text "The label " :<>: ShowType l :<>: Text " is not in " :<>: ShowType r)

type family LacksR (l :: Symbol) (r :: [LT k]) (r_orig :: [LT k]) :: Constraint where
  LacksR l '[] _ = Unconstrained
  LacksR l (l :-> t ': x) r = TypeError (Text "The label " :<>: ShowType l :<>: Text " already exists in " :<>: ShowType r)
  LacksR l (l' :-> _ ': x) r = Ifte (l <=.? l') Unconstrained (LacksR l x r)

type family Merge (l :: [LT k]) (r :: [LT k]) :: [LT k] where
  Merge '[] r = r
  Merge l '[] = l
  Merge (h :-> a ': tl) (h :-> b ': tr) = TypeError (Text "The label " :<>: ShowType h :<>: Text " has conflicting assignments." :$$: Text "Its type is both " :<>: ShowType a :<>: Text " and " :<>: ShowType b :<>: Text ".")
  Merge (hl :-> al ': tl) (hr :-> ar ': tr) = Ifte (hl <=.? hr) (hl :-> al ': Merge tl (hr :-> ar ': tr)) (hr :-> ar ': Merge (hl :-> al ': tl) tr)

type family MinJoinR (l :: [LT k]) (r :: [LT k]) :: [LT k] where
  MinJoinR '[] r = r
  MinJoinR l '[] = l
  MinJoinR (h :-> a ': tl) (h :-> a ': tr) = (h :-> a ': MinJoinR tl tr)
  MinJoinR (h :-> a ': tl) (h :-> b ': tr) = TypeError (Text "The label " :<>: ShowType h :<>: Text " has conflicting assignments." :$$: Text "Its type is both " :<>: ShowType a :<>: Text " and " :<>: ShowType b :<>: Text ".")
  MinJoinR (hl :-> al ': tl) (hr :-> ar ': tr) = Ifte (CmpSymbol hl hr == 'LT) (hl :-> al ': MinJoinR tl (hr :-> ar ': tr)) (hr :-> ar ': MinJoinR (hl :-> al ': tl) tr)

-- | Returns the left list with all of the elements from the right list removed.
type family Diff (l :: [LT k]) (r :: [LT k]) :: [LT k] where
  Diff '[] r = '[]
  Diff l '[] = l
  Diff (l :-> al ': tl) (l :-> al ': tr) = Diff tl tr
  Diff (hl :-> al ': tl) (hr :-> ar ': tr) = Ifte (hl <=.? hr) (hl :-> al ': Diff tl (hr :-> ar ': tr)) (Diff (hl :-> al ': tl) tr)

type family Ifte (c :: Bool) (t :: k) (f :: k) :: k where
  Ifte True t f = t
  Ifte False t f = f

-- | There doesn't seem to be a (<=.?) :: Symbol -> Symbol -> Bool,
-- so here it is in terms of other ghc-7.8 type functions
type a <=.? b = (CmpSymbol a b == 'LT)
