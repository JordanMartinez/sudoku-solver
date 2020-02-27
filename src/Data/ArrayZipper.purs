module Data.ArrayZipper where

import Prelude

import Data.Array (length, unsafeIndex)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Partial.Unsafe (unsafePartial)

newtype ArrayZipper a = ArrayZipper { array :: Array a, focusIndex :: Int, maxIndex :: Int }

derive instance functorArrayZipper :: Functor ArrayZipper

instance foldableArrayZipper :: Foldable ArrayZipper where
  foldl f init (ArrayZipper r) = foldl f init r.array

  foldr f last (ArrayZipper r) = foldr f last r.array

  foldMap = foldMapDefaultL

instance traversableArrayZipper :: Traversable ArrayZipper where
  traverse f (ArrayZipper r) = ado
    ar <- traverse f r.array
    in (ArrayZipper r { array = ar })

  sequence = sequenceDefault

asArrayZipper :: forall a. a -> ArrayZipper a
asArrayZipper a = ArrayZipper { array: [a], focusIndex: 0, maxIndex: 0 }

toArrayZipperFirst :: forall a. Array a -> Maybe (ArrayZipper a)
toArrayZipperFirst = case _ of
  [] -> Nothing
  array -> Just (ArrayZipper { array, focusIndex: 0, maxIndex: length array - 1 })

toArrayZipperLast :: forall a. Array a -> Maybe (ArrayZipper a)
toArrayZipperLast = case _ of
  [] -> Nothing
  array ->
    let maxIndex = length array - 1
    in Just (ArrayZipper { array, focusIndex: maxIndex, maxIndex })

toArrayZipperAt :: forall a. Int -> Array a -> Maybe (ArrayZipper a)
toArrayZipperAt startingFocusIndex = case _ of
  [] -> Nothing
  array ->
    let
      maxIndex = length array - 1
      focusIndex = clamp 0 maxIndex startingFocusIndex
    in Just (ArrayZipper { array, focusIndex, maxIndex })

hasPrev :: forall a. ArrayZipper a -> Boolean
hasPrev (ArrayZipper r) = r.focusIndex > 0

hasNext :: forall a. ArrayZipper a -> Boolean
hasNext (ArrayZipper r) = r.focusIndex < r.maxIndex

prev :: forall a. ArrayZipper a -> Maybe (ArrayZipper a)
prev (ArrayZipper r)
  | r.focusIndex - 1 >= 0 = Just (ArrayZipper r { focusIndex = r.focusIndex - 1 })
  | otherwise = Nothing

next :: forall a. ArrayZipper a -> Maybe (ArrayZipper a)
next (ArrayZipper r)
  | r.focusIndex + 1 <= r.maxIndex = Just (ArrayZipper r { focusIndex = r.focusIndex + 1 })
  | otherwise = Nothing

shiftBy :: forall a. (Int -> Int) -> ArrayZipper a -> Maybe (ArrayZipper a)
shiftBy f (ArrayZipper r) =
  let updatedFocusIndex = f r.focusIndex
  in if updatedFocusIndex > 0 && updatedFocusIndex <= r.maxIndex
      then Just (ArrayZipper r { focusIndex = updatedFocusIndex })
      else Nothing

shiftToFirst :: forall a. ArrayZipper a -> ArrayZipper a
shiftToFirst (ArrayZipper r) = ArrayZipper r { focusIndex = 0 }

shiftToLast :: forall a. ArrayZipper a -> ArrayZipper a
shiftToLast (ArrayZipper r) = ArrayZipper r { focusIndex = r.maxIndex }

foreign import unsafeInsertAt :: forall a. Int -> a -> Array a -> Array a
foreign import unsafeSetAt :: forall a. Int -> a -> Array a -> Array a
foreign import unsafeModifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a

getFocus :: forall a. ArrayZipper a -> a
getFocus (ArrayZipper r) = unsafePartial (unsafeIndex r.array r.focusIndex)

setFocus :: forall a. a -> ArrayZipper a -> ArrayZipper a
setFocus a (ArrayZipper r) = ArrayZipper (r { array = unsafeSetAt r.focusIndex a r.array })

modifyFocus :: forall a. (a -> a) -> ArrayZipper a -> ArrayZipper a
modifyFocus f (ArrayZipper r) = ArrayZipper (r { array = unsafeModifyAt r.focusIndex f r.array })

pushPrev :: forall a. a -> ArrayZipper a -> ArrayZipper a
pushPrev a (ArrayZipper r) =
  ArrayZipper { focusIndex: r.focusIndex + 1
              , maxIndex: r.maxIndex + 1
              , array: unsafeInsertAt r.focusIndex a r.array
              }

pushNext :: forall a. a -> ArrayZipper a -> ArrayZipper a
pushNext a (ArrayZipper r) =
  ArrayZipper r { maxIndex = r.maxIndex + 1
                , array = unsafeInsertAt (r.focusIndex + 1) a r.array
                }

shiftFocusPrev :: forall a. a -> ArrayZipper a -> ArrayZipper a
shiftFocusPrev a (ArrayZipper r) =
    ArrayZipper r { maxIndex = r.maxIndex + 1
                  , array = unsafeInsertAt r.focusIndex a r.array
                  }

shiftFocusNext :: forall a. a -> ArrayZipper a -> ArrayZipper a
shiftFocusNext a (ArrayZipper r) =
    ArrayZipper r { focusIndex = r.focusIndex
                  , maxIndex = r.maxIndex + 1
                  , array = unsafeInsertAt (r.focusIndex + 1) a r.array
                  }
