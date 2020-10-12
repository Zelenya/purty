module Where where

foo = withoutType1
  where
    withoutType1 = 1
    withoutType2 = 1

    withType1 = 3
    withType1 :: Int

    withoutType3 = 1

    withType2 :: Int -> Int
    withType2 1 = 41
    withType2 _ = 42
