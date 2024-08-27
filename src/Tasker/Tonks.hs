module Tasker.Tonks where

data Column n a
data Row ts
data (:+:) a b = a :+: b
data Dataset ts = Dataset (Row ts)
data Series n a = Series (Column n a)

data Tensor d a
data Dataset a
data Event
type Predicted d = Tensor d Float
type Actual d = Tensor d Float
type Cost = Tensor 1 Float

type Loss = Actual d -> Predicted d -> Cost
-- type Target = Costumer -> Dataset Event -> Dataset (Event :+: Column "target" 


type CustomerCol = Column "customer" Costumer
type TimestampCol = Column "timestamp" Datetime
type EventCol = Column "event" Event
type EventRow = Row (CustomerCol :+: TimestampCol :+: EventCol)
type EventSet = Dataset EventRow

class Name a where
class Contains c a ts where
class NotContains c a ts where

getColumn :: (Name n, Contains n a ts) => Dataset ts -> Series n a
addColumn :: (Name n, NotContains n a ts, Contains n a ts') => Series n a -> Dataset ts -> Dataset ts'
withColumn :: (Name n, NotContains n a ts, Contains n a ts') => (Dataset ts -> Series n a) -> Dataset ts -> Dataset ts'

type Target = EventSet -> Series "target" Float