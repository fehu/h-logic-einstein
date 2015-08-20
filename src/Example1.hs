{-# LANGUAGE MultiParamTypeClasses #-}

module Example1 (
  main
) where

import Data.List (intercalate)

import Problem
import Problem.DSL.Internal

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

data AnEntry = AnEntry ID
                       (Maybe Color)
                       (Maybe Nacion)
                       (Maybe Animal)
                       (Maybe Bebida)
                       (Maybe Musica)

instance IdRepr ID where getRepr = show
                         getOrd  = fromEnum

instance Entry   AnEntry where get _ = id
instance EntryId AnEntry where getId (AnEntry i _ _ _ _ _) = Id i

instance EntryGet AnEntry ID      where getV _ (AnEntry i _ _ _ _ _) = Just i
instance EntryGet AnEntry Color   where getV _ (AnEntry _ c _ _ _ _) = c
instance EntryGet AnEntry Nacion  where getV _ (AnEntry _ _ n _ _ _) = n
instance EntryGet AnEntry Animal  where getV _ (AnEntry _ _ _ a _ _) = a
instance EntryGet AnEntry Bebida  where getV _ (AnEntry _ _ _ _ b _) = b
instance EntryGet AnEntry Musica  where getV _ (AnEntry _ _ _ _ _ m) = m


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
        , "№6"  -:    Verde <==> Marfil |?> aDerechaDe |:: "La casa verde está junto y a la derecha de la casa de marfil."
        , "№9"  -:        C <==> Leche
        , "№11" -: Guitarra <==> Zorro  |?> enseguida
        ]

type CondFunc1 v = Maybe v -> Maybe v -> Bool

enseguida :: CondFunc1 ID
Just x `enseguida` Just y  = succ x == y || pred x == y

aDerechaDe :: CondFunc1 ID
Just x `aDerechaDe` Just y = succ y == x

aIzquierdaDe = flip aDerechaDe


--         , Verde <--> Cafe           --  4
--         , Ruso <--> Te              --  5

--         , Piano <--> Caracoles      --  7
--         , Amarilla <--> Bateria     --  8

--         , A <--> Noruego            -- 10

--         , enseguida Caballo Bateria -- 12
--         , Violin <--> Naranjada     -- 13
--         , Japones <--> Teclado      -- 14
--         , enseguida Noruego Azul    -- 15
--         , constr16                  -- 16
--         , enseguida Perro Leche     -- 17
--         ]




main :: IO()
main = putStrLn $ intercalate "\n" (map show facts)

