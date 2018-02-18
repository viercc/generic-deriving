Experiment of possible Generics extensions.

See [Original README.md](https://github.com/viercc/generic-deriving/blob/experiment-generic-arr/README-original.md),
which explains 'generic-deriving' package it is based on.

See pre-existing discussion.

 * [#8516](https://ghc.haskell.org/trac/ghc/ticket/8516)

## Motivation

Current implementation of `Generic1` has no way to handle
any type including `f p -> g p`, unless parameter `p` is phantom in
`f`. For example, following declaration is valid for `Reader` but
is invalid for `Cont`. Thus you can't derive `GFunctor (Cont r)`
automatically, even though `deriving Functor` works for both.

```
newtype Reader r a = Reader { runReader :: r -> a }
  deriving (Functor, Generic1)

newtype Cont r a   = Cont { runCont :: (a -> r) -> r }
  deriving (Functor, Generic1)
```

And there can't be `GContravariant` class to automatically
derive [Contravariant](https://hackage.haskell.org/package/contravariant)
instances, even for very simple one like this.

```
newtype Predicate a = Predicate (a -> Bool)
  deriving (Generic1)
```

These are due to how Generic1 represents your type.
Ignoring metadata, current implementation is this:

```
data T a b p =
   T1 a p
 | T2 (Maybe [T a b p])
 | T3 ((a, b) -> p)

type instance Rep1 (T a b) =
  {- Constructor T1 -}
      {- Field of T1 #1 -}
           K1 a
      {- Field of T1 #2 -}
       :*: Par1
  {- Constructor T2 -}
  :+: {- Field of T2 #1 -}
      -- If the field can be thought of composition of some
      -- type constructors @f (g (... (h p))@
      -- and parameter @p@ do not appear in @f, g, ...@,
      -- it is represented by @f :.: g :.: ... :.: h@.
      -- In this case, @f = Maybe; g = []; h = Rec1 (T a b)@.
      Maybe :.: [] :.: Rec1 (T a b)
  {- Constructor T3 -}
  :+: {- Field of T3 #1 -}
      -- There is no special treatment to @->@.
      -- @x -> y@ is merely treated as some
      -- type constructor @(->)@ applied to @x@ and @y@.
      ((->) (a, b)) :.: Par1
```

Notice how the arrow of `(a, b) -> p` is treated. This is why
Generics fail on `p -> Bool`. It's same reason to reject
`SomeType p Bool` but to accept `SomeType Bool p`.

So the motivation is: **How can we do better if it treat (->)
specially?**

## Method

Introduce new type `:->:` to base representation of type,
as a new friend of `K1`, `Par1`, `:+:`, `:*:`, ...

```
newtype (:->:) f g p = Arr1 { unArr1 :: f p -> g p }
```

When `deriving Generic1` tries to derive a representation of type `t1 -> t2`
which includes parameter type variable `p`,
it finds `f` and `g` such that `t1 -> t2 = (f :->: g) p`.

Above example do not change much in new method.


```
data T a b p =
   T1 a p
 | T2 (Maybe [T a b p])
 | T3 ((a, b) -> p)

type instance Rep1 (T a b) =
       K1 a :*: Par1
  :+: Maybe :.: [] :.: Rec1 (T a b)
  :+: K1 (a, b) :->: Par1
```

And this method works for `Cont` example no problem.

```
newtype Cont r a   = Cont { runCont :: (a -> r) -> r }

type instance Rep1 (Cont r) =
    (Par1 :->: K1 r) :->: K1 r
```

How do we derive `GFunctor (Cont r)`,
when `Rep1 (Cont r)` involves new friend `:->:`?

```
class GFunctor f where
  gmap :: (a -> b) -> f a -> f b

class GContravariant f where
  gcontramap :: (a -> b) -> f b -> f a

-- | K1 is BOTH GFunctor and GContravariant.
instance GFunctor (K1 c) where
  gmap _ (K1 c) = K1 c

instance GContravariant (K1 c) where
  gcontramap _ (K1 c) = K1 c

-- | Par1 is GFunctor, but not GContravariant.
instance GFunctor Par1 where
  gmap q (Par1 p) = Par1 (q p)

-- | (contravariant :->: covariant) = covariant
instance (GContravariant f, GFunctor g) => GFunctor (f :->: g) where
  gmap q (Arr1 r) = Arr1 $ gmap q . r . gcontramap q
    {-
    q :: a -> b
    r :: f a -> g a
    gmap q       :: g a -> g b
    gcontramap q :: f b -> f a
    gmap q . r . gcontramap q :: f b -> g b
    -}

-- | (covariant :->: contravariant) = contravariant
instance (GFunctor f, GContravariant g) => GContravariant (f :->: g) where
  gcontramap q (Arr1 r) = Arr1 $ gcontramap q . r . gmap q
```

These instance gives you `GFunctor ((Par1 :-> K1 r) :-> K1 r)` and it works as expected.

