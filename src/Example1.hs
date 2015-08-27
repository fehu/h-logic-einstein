{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
         #-}

module Example1 (
  main
) where

import Data.List (intercalate)
import Control.Arrow ((&&&))

import Problem
import Problem.Exec

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data ID = A | B | C | D | E
    deriving (Show, Eq, Ord, Enum, Bounded)

data Color  = Roja | Verde | Azul | Marfil | Amarilla
    deriving (Eq, Show)

data Nacion = Ingles | Espanol | Ruso | Noruego | Japones
    deriving (Eq, Show)

data Animal = Perro | Caracoles | Zorro | Caballo | Cebra
    deriving (Eq, Show)

data Bebida = Cafe | Te | Leche | Naranjada | Agua
    deriving (Eq, Show)

data Musica = Piano | Bateria | Guitarra | Teclado | Violin
    deriving (Eq, Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data AnEntry = AnEntry { casaId :: ID
                       , color  :: Maybe Color
                       , nacion :: Maybe Nacion
                       , animal :: Maybe Animal
                       , bebida :: Maybe Bebida
                       , musica :: Maybe Musica
                       }
             deriving Show


instance IdRepr ID where getRepr = show
                         getOrd  = fromEnum

instance Entry   AnEntry where
    getId (AnEntry i _ _ _ _ _) = Id i

instance EntryGet AnEntry ID      where getV _ = Just . casaId
                                        setV _ _ = Nothing
                                        clearV _ _ = Nothing
instance EntryGet AnEntry Color   where getV _ = color
                                        setV e a = Just $ e {color  = Just a}
                                        clearV _ e = Just $ e {color = Nothing}
instance EntryGet AnEntry Nacion  where getV _ = nacion
                                        setV e a = Just $ e {nacion = Just a}
                                        clearV _ e = Just $ e {color = Nothing}
instance EntryGet AnEntry Animal  where getV _ = animal
                                        setV e a = Just $ e {animal = Just a}
                                        clearV _ e = Just $ e {color = Nothing}
instance EntryGet AnEntry Bebida  where getV _ = bebida
                                        setV e a = Just $ e {bebida = Just a}
                                        clearV _ e = Just $ e {color = Nothing}
instance EntryGet AnEntry Musica  where getV _ = musica
                                        setV e a = Just $ e {musica = Just a}
                                        clearV _ e = Just $ e {color = Nothing}

instance (Accessible v, EntryGet AnEntry v) => EntryAccessible AnEntry v
    where updateEntry  = flip setV
          clearEntry v = clearV (varDescriptor v)


instance Accessible ID     where modifiable _    = False
                                 varDescriptor _ = AccessibleDescriptor "ID"
instance Accessible Color  where modifiable _    = True
                                 varDescriptor _ = AccessibleDescriptor "Color"
instance Accessible Nacion where modifiable _    = True
                                 varDescriptor _ = AccessibleDescriptor "Nacion"
instance Accessible Animal where modifiable _    = True
                                 varDescriptor _ = AccessibleDescriptor "Animal"
instance Accessible Bebida where modifiable _    = True
                                 varDescriptor _ = AccessibleDescriptor "Bebida"
instance Accessible Musica where modifiable _    = True
                                 varDescriptor _ = AccessibleDescriptor "Musica"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

facts :: KnownFacts AnEntry
facts = [ "№2"  -:   Ingles <==> Roja                  |:: "El inglés vive en la casa roja."
        , "№3"  -:  Espanol <==> Perro                 |:: "El español es el proprietario del perro."
        , "№4"  -:    Verde <==> Cafe                  |:: "En la casa verde se bebe café."
        , "№5"  -:     Ruso <==> Te                    |:: "El ruso bebe té."
        , "№6"  -:    Verde <==> Marfil |?> aDerechaDe |:: "La casa verde está junto y a la derecha de la casa de marfil."
        , "№7"  -:    Piano <==> Caracoles             |:: "El pianista tiene caracoles."
        , "№8"  -: Amarilla <==> Bateria               |:: "En la casa amarilla se toca la batería."
        , "№9"  -:        C <==> Leche                 |:: "En la casa del centro se vende leche."
        , "№10" -:  Noruego <==> A                     |:: "El noruego vive en la primera casa de la izquierda."
        , "№11" -: Guitarra <==> Zorro   |?> enseguida |:: "El hombre que toca guitarra vive en la casa" ++
                                                           " contigua a la del dueño del zorro."
        , "№12" -:  Caballo <==> Bateria |?> enseguida |:: "En la casa contigua a aquella donde se encuentra el caballo" ++
                                                           " se toca la batería."
        , "№13" -:   Violin <==> Naranjada             |:: "El violinista bebe naranjada."
        , "№14" -:  Japones <==> Teclado               |:: "El japonés toca el teclado."
        , "№15" -:  Noruego <==> Azul    |?> enseguida |:: "El noruego vive a lado de la casa azul."
        , "№16" -:  Japones  !?  tieneUnSoloVicino     |:: "El japonés solo tiene un vicino."
        , "№17" -:    Perro <==> Leche   |?> enseguida |:: "El quien tiene perro vive junto al quien toma leche."
        ]

type CondFunc1 v = Maybe v -> Maybe v -> Bool

enseguida :: CondFunc1 ID
Just x `enseguida` Just y  = x /= y &&
                             ((maxBound :: ID) /= x && succ x == y ||
                              (minBound :: ID) /= x && pred x == y )

aDerechaDe :: CondFunc1 ID
Just x `aDerechaDe` Just y = x /= y && (maxBound :: ID) /= y && succ y == x

aIzquierdaDe = flip aDerechaDe

tieneUnSoloVicino :: Maybe ID -> Bool
tieneUnSoloVicino (Just id) = id == maxBound || id == minBound

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newEntry i = AnEntry i Nothing Nothing Nothing Nothing Nothing

table = newETable (Id &&& newEntry) (enumFrom A)


instance EntryValExt AnEntry where setValue   (Value v) = updateEntry v
                                   clearValue (Value v) = clearEntry  v

res1 = applyRules facts table

ctx :: ExecContext Rule AnEntry
ctx = newExecContext table

res2 = solveProblem ctx facts (Just 100)

main :: IO()
main = do putStrLn "facts:"
          putStrLn $ intercalate "\n" (map show facts)
--          putStrLn "\n-- table: "
--          print table

--          putStrLn "== apply rules =="
--          putStrLn "-- history: "
--          putStrLn . showHistory . snd $ res1
--          putStrLn "-- table: "
--          print $ fst res1

          putStrLn "== run solveProblem =="
          let (c', r', a') = res2
          putStrLn "-- history:"
          putStrLn $ showHistory a'
          putStrLn "-- context:"
          print c'
          putStrLn "-- result:"
          print r'









