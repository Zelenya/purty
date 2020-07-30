module Deriving where

data MyData = MyDataOne | MyDataTwo String

derive instance myDataShow :: Show MyData
derive instance myDataWriteJson :: WriteJSON MyData
