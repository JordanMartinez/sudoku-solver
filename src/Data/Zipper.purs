module Data.Zipper where

import Prelude

import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault, traverse)

newtype Zipper a = Zipper { prev :: List a, focus :: a, next :: List a }

derive instance functorZipper :: Functor Zipper

instance foldableZipper :: Foldable Zipper where
  foldl f init (Zipper r) = end
    where
      start = foldl f init (reverse r.prev)
      middle = f start r.focus
      end = foldl f middle r.next

  foldr f last (Zipper r) = start
    where
      end = foldr f last r.next
      middle = f r.focus end
      start = foldr f middle r.prev

  foldMap = foldMapDefaultL

instance traversableZipper :: Traversable Zipper where
  traverse f (Zipper r) = ado
    start <- traverse f (reverse r.prev)
    middle <- f r.focus
    end <- traverse f r.next
    in (Zipper { prev: reverse start, focus: middle, next: end })

  sequence = sequenceDefault

asZipper :: forall a. a -> Zipper a
asZipper a = Zipper { prev: Nil, focus: a, next: Nil }

toZipper :: forall a. List a -> Maybe (Zipper a)
toZipper = case _ of
  Nil -> Nothing
  Cons head tail -> Just (Zipper { prev: Nil, focus: head, next: tail })

toReversedZipper :: forall a. List a -> Maybe (Zipper a)
toReversedZipper = case _ of
  Nil -> Nothing
  Cons head tail -> Just (Zipper { prev: tail, focus: head, next: Nil })

hasPrev :: forall a. Zipper a -> Boolean
hasPrev (Zipper r) = case r.prev of
  Nil -> false
  _ -> true

hasNext :: forall a. Zipper a -> Boolean
hasNext (Zipper r) = case r.next of
  Nil -> false
  _ -> true

prev :: forall a. Zipper a -> Maybe (Zipper a)
prev (Zipper r) = case r.prev of
  Nil -> Nothing
  Cons h tail -> Just (Zipper { prev: tail, focus: h, next: r.focus : r.next })

next :: forall a. Zipper a -> Maybe (Zipper a)
next (Zipper r) = case r.next of
  Nil -> Nothing
  Cons h tail -> Just (Zipper { prev: r.focus : tail, focus: h, next: tail })

getFocus :: forall a. Zipper a -> a
getFocus (Zipper r) = r.focus

setFocus :: forall a. a -> Zipper a -> Zipper a
setFocus a (Zipper r) = Zipper (r { focus = a})

modifyFocus :: forall a. (a -> a) -> Zipper a -> Zipper a
modifyFocus f (Zipper r) = Zipper (r { focus = f (r.focus) })

pushPrev :: forall a. a -> Zipper a -> Zipper a
pushPrev a (Zipper r) = Zipper (r { prev = a : r.prev})

pushNext :: forall a. a -> Zipper a -> Zipper a
pushNext a (Zipper r) = Zipper (r { next = a : r.next })

shiftFocusPrev :: forall a. a -> Zipper a -> Zipper a
shiftFocusPrev a (Zipper r) = Zipper (r { prev = r.focus : r.prev, focus = a })

shiftFocusNext :: forall a. a -> Zipper a -> Zipper a
shiftFocusNext a (Zipper r) = Zipper (r { focus = a, next = r.focus : r.next })
