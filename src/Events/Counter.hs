module Events.Counter where


import qualified Eventful

newtype Counter = Counter { counter :: Int}
    deriving (Show, Eq)

data CounterEvent
    = CounterReset
    | CounterIncreased Int
    | CounterDecreased Int
    deriving (Show, Eq)


unpack:: Counter -> Int
unpack (Counter count) = count


handleCountEvent:: Counter -> CounterEvent -> Counter
handleCountEvent (Counter count) (CounterIncreased amount) = Counter ( count + amount)
handleCountEvent (Counter count) (CounterDecreased amount) = Counter ( count - amount)
handleCountEvent _ CounterReset = Counter 0


counterProjection:: Eventful.Projection Counter CounterEvent
counterProjection = Eventful.Projection
    { Eventful.projectionSeed = Counter 0
    , Eventful.projectionEventHandler = handleCountEvent
    }


latestCounterState:: Foldable t => t CounterEvent -> Counter
latestCounterState =
    Eventful.latestProjection counterProjection


allCounterState:: [CounterEvent] -> [Counter]
allCounterState =
    Eventful.allProjections counterProjection
