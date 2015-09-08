module Example2 ( main ) where

import Data.List (intercalate)

import Control.Arrow ((&&&))

import LogicProblem

{- Ships Puzzle

    from https://www.mathsisfun.com/puzzles/ships.html


There are 5 ships in a port:

1. The Greek ship leaves at six and carries coffee.
2. The Ship in the middle has a black chimney.
3. The English ship leaves at nine.
4. The French ship with blue chimney is to the left of a ship that carries coffee.
5. To the right of the ship carrying cocoa is a ship going to Marseille.
6. The Brazilian ship is heading for Manila.
7. Next to the ship carrying rice is a ship with a green chimney.
8. A ship going to Genoa leaves at five.
9. The Spanish ship leaves at seven and is to the right of the ship going to Marseille.
10. The ship with a red chimney goes to Hamburg.
11. Next to the ship leaving at seven is a ship with a white chimney.
12. The ship on the border carries corn.
13. The ship with a black chimney leaves at eight.
14. The ship carrying corn is anchored next to the ship carrying rice.
15. The ship to Hamburg leaves at six.

Which ship goes to Port Said? Which ship carries tea?
-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


data Position = Position Int deriving (Show, Eq, Ord)

instance Bounded Position where maxBound = Position 5
                                minBound = Position 1

instance Enum Position where toEnum i | i >= min && i <= max = Position i
                                      | otherwise            = error $ "Position out of range: "
                                                                ++ show i
                                where (Position min) = minBound
                                      (Position max) = maxBound
                             fromEnum (Position i) = i


data Country = Greek | English | French | Brazilian | Spanish deriving (Show, Eq)

data Destination = Marseille | Manila | Genoa | Hamburg | PortSaid deriving (Show, Eq)

data Cargo = Coffee | Cocoa | Rice | Corn | Tea deriving (Show, Eq)

data Color = Black | Blue | Green | Red | White deriving (Show, Eq)

data Leaves = At Int deriving (Show, Eq) -- At5 | At6 | At7 | At8 | At9

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

facts :: KnownFacts AnEntry
facts = rules [
   "1a" -:      Greek <==> At 6                      |:: "The Greek ship leaves at six ..."
 , "1b" -:      Greek <==> Coffee                    |:: "... and carries coffee."
 , "1c" -:       At 6 <==> Coffee
 , "2"  -: Position 3 <==> Black                     |:: "The Ship in the middle has a black chimney."
 , "3"  -:    English <==> At 9                      |:: "The English ship leaves at nine."
 , "4a" -:     French <==> Blue                      |:: "The French ship with blue chimney..."
 , "4b" -:     French <==> Coffee    <?| toTheRight  |:: "... is to the left of a ship that carries coffee."
 , "4c" -:       Blue <==> Coffee    <?| toTheRight
 , "5"  -:      Cocoa <==> Marseille <?| toTheRight  |:: "To the right of the ship carrying cocoa" ++
                                                         " is a ship going to Marseille."
 , "6"  -:  Brazilian <==> Manila                    |:: "The Brazilian ship is heading for Manila."
 , "7"  -:       Rice <==> Green     |?> nextTo      |:: "Next to the ship carrying rice is a ship with a green chimney."
 , "8"  -:      Genoa <==> At 5                      |:: "A ship going to Genoa leaves at five."
 , "9a" -:    Spanish <==> At 7                      |:: "The Spanish ship leaves at seven ..."
 , "9b" -:    Spanish <==> Marseille |?> toTheRight  |:: "... and is to the right of the ship going to Marseille."
 , "9c" -:       At 7 <==> Marseille |?> toTheRight
 , "10" -:        Red <==> Hamburg                   |:: "The ship with a red chimney goes to Hamburg."
 , "11" -:       At 7 <==> White     |?> nextTo      |:: "Next to the ship leaving at seven is a ship with a white chimney."
 , "12" -:       Corn  !?  onTheBorder               |:: "The ship on the border carries corn."
 , "13" -:      Black <==> At 8                      |:: "The ship with a black chimney leaves at eight."
 , "14" -:       Corn <==> Rice      |?> nextTo      |:: "The ship carrying corn is anchored next to the ship carrying rice."
 , "15" -:    Hamburg <==> At 6                      |:: "The ship to Hamburg leaves at six."
 ]

Just x `toTheRight` Just y = x /= y && (maxBound :: Position) /= y && succ y == x

Just x `nextTo` Just y  = x /= y &&
    ( (maxBound :: Position) /= x && succ x == y || minBound /= x && pred x == y )

onTheBorder (Just pos) = pos == (maxBound :: Position) || pos == minBound


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance Accessible Position    where modifiable _ = False
                                      varDescriptor _ = AccessibleDescriptor "Position"
instance Accessible Country     where modifiable _ = True
                                      varDescriptor _ = AccessibleDescriptor "Country"
instance Accessible Leaves      where modifiable _ = True
                                      varDescriptor _ = AccessibleDescriptor "Leaves"
instance Accessible Destination where modifiable _ = True
                                      varDescriptor _ = AccessibleDescriptor "Destination"
instance Accessible Cargo       where modifiable _ = True
                                      varDescriptor _ = AccessibleDescriptor "Cargo"
instance Accessible Color       where modifiable _ = True
                                      varDescriptor _ = AccessibleDescriptor "Color"


instance IdRepr Position where getOrd (Position i) = i
                               getRepr             = show . getOrd


data AnEntry = AnEntry { position    :: Position
                       , country     :: Maybe Country
                       , destination :: Maybe Destination
                       , cargo       :: Maybe Cargo
                       , color       :: Maybe Color
                       , leavesAt    :: Maybe Leaves
                       }
             deriving Show

instance Entry AnEntry where getId (AnEntry i _ _ _ _ _) = Id i

instance AccessibleEntry AnEntry Position      where getV _   = Just . position
                                                     setV _   = const Nothing
                                                     clearV _ = const Nothing
instance AccessibleEntry AnEntry Country       where getV _     = country
                                                     setV e x   = Just $ e {country = Just x}
                                                     clearV _ e = Just $ e {country = Nothing}
instance AccessibleEntry AnEntry Leaves        where getV _     = leavesAt
                                                     setV e x   = Just $ e {leavesAt = Just x}
                                                     clearV _ e = Just $ e {leavesAt = Nothing}
instance AccessibleEntry AnEntry Destination   where getV _     = destination
                                                     setV e x   = Just $ e {destination = Just x}
                                                     clearV _ e = Just $ e {destination = Nothing}
instance AccessibleEntry AnEntry Cargo         where getV _     = cargo
                                                     setV e x   = Just $ e {cargo = Just x}
                                                     clearV _ e = Just $ e {cargo = Nothing}
instance AccessibleEntry AnEntry Color         where getV _     = color
                                                     setV e x   = Just $ e {color = Just x}
                                                     clearV _ e = Just $ e {color = Nothing}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newEntry pos = AnEntry pos Nothing Nothing Nothing Nothing Nothing

table = newETable (Id &&& newEntry) (enumFromTo minBound maxBound)

ctx :: ExecContext AtomicRule AnEntry
ctx = newExecContext table


res = solveProblem ctx facts Nothing  --(Just 100)

main :: IO()
main = do putStrLn "facts:"
          putStrLn $ intercalate "\n" (map show facts)

          putStrLn "== run solveProblem =="
          let (c', r', a') = res
          putStrLn "-- history:"
          putStrLn $ showHistory a'
          putStrLn "-- context:"
          print c'
          putStrLn "-- result:"
          print r'



