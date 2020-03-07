module Data.ListZipper where

import Prelude

import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault, traverse)

newtype ListZipper a = ListZipper { prev :: List a, focus :: a, next :: List a }

derive instance functorListZipper :: Functor ListZipper

instance foldableListZipper :: Foldable ListZipper where
  foldl f init (ListZipper r) = end
    where
      start = foldl f init (reverse r.prev)
      middle = f start r.focus
      end = foldl f middle r.next

  foldr f last (ListZipper r) = start
    where
      end = foldr f last r.next
      middle = f r.focus end
      start = foldr f middle r.prev

  foldMap = foldMapDefaultL

instance traversableListZipper :: Traversable ListZipper where
  traverse f (ListZipper r) = ado
    start <- traverse f (reverse r.prev)
    middle <- f r.focus
    end <- traverse f r.next
    in (ListZipper { prev: reverse start, focus: middle, next: end })

  sequence = sequenceDefault

asListZipper :: forall a. a -> ListZipper a
asListZipper a = ListZipper { prev: Nil, focus: a, next: Nil }

toListZipper :: forall a. List a -> Maybe (ListZipper a)
toListZipper = case _ of
  Nil -> Nothing
  Cons head tail -> Just (ListZipper { prev: Nil, focus: head, next: tail })

toReversedListZipper :: forall a. List a -> Maybe (ListZipper a)
toReversedListZipper = case _ of
  Nil -> Nothing
  Cons head tail -> Just (ListZipper { prev: tail, focus: head, next: Nil })

hasPrev :: forall a. ListZipper a -> Boolean
hasPrev (ListZipper r) = case r.prev of
  Nil -> false
  _ -> true

hasNext :: forall a. ListZipper a -> Boolean
hasNext (ListZipper r) = case r.next of
  Nil -> false
  _ -> true

prev :: forall a. ListZipper a -> Maybe (ListZipper a)
prev (ListZipper r) = case r.prev of
  Nil -> Nothing
  Cons h tail -> Just (ListZipper { prev: tail, focus: h, next: r.focus : r.next })

next :: forall a. ListZipper a -> Maybe (ListZipper a)
next (ListZipper r) = case r.next of
  Nil -> Nothing
  Cons h tail -> Just (ListZipper { prev: r.focus : tail, focus: h, next: tail })

getFocus :: forall a. ListZipper a -> a
getFocus (ListZipper r) = r.focus

setFocus :: forall a. a -> ListZipper a -> ListZipper a
setFocus a (ListZipper r) = ListZipper (r { focus = a})

modifyFocus :: forall a. (a -> a) -> ListZipper a -> ListZipper a
modifyFocus f (ListZipper r) = ListZipper (r { focus = f (r.focus) })

pushPrev :: forall a. a -> ListZipper a -> ListZipper a
pushPrev a (ListZipper r) = ListZipper (r { prev = a : r.prev})

pushNext :: forall a. a -> ListZipper a -> ListZipper a
pushNext a (ListZipper r) = ListZipper (r { next = a : r.next })

shiftFocusPrev :: forall a. a -> ListZipper a -> ListZipper a
shiftFocusPrev a (ListZipper r) = ListZipper (r { prev = r.focus : r.prev, focus = a })

shiftFocusNext :: forall a. a -> ListZipper a -> ListZipper a
shiftFocusNext a (ListZipper r) = ListZipper (r { focus = a, next = r.focus : r.next })
