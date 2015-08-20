{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
         #-}


module ProblemStatement.Example1 (

) where

import ProblemStatement

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

data AnEntry = AnEntry (Maybe ID)
                       (Maybe Color)
                       (Maybe Nacion)
                       (Maybe Animal)
                       (Maybe Bebida)
                       (Maybe Musica)

instance Entry AnEntry where


instance EntryGet AnEntry ID
instance EntryGet AnEntry Color
instance EntryGet AnEntry Nacion
instance EntryGet AnEntry Animal
instance EntryGet AnEntry Bebida
instance EntryGet AnEntry Musica

--    get (AccessibleDescriptor "ID") = undefined
--    get vd (AnEntry id color nacion animal bebida musica) =
--        case vd of ((AccessibleDescriptor "ID") :: AccessibleDescriptor ID) -> id


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
facts = [ "№2" -:  Ingles <==> Roja                  |:: "El inglés vive en la casa roja."
        , "№3" -: Espanol <==> Perro                 |:: "El español es el proprietario del perro."
        , "№6" -:   Verde <==> Marfil |?> aDerechaDe |:: "La casa verde está junto y a la derecha de la casa de marfil."
        ]

--hechos = [ Ingles <--> Roja          --  2
--         , Espanol <--> Perro        --  3
--         , Verde <--> Cafe           --  4
--         , Ruso <--> Te              --  5
--         , cond6                     --  6
--         , Piano <--> Caracoles      --  7
--         , Amarilla <--> Bateria     --  8
--         , C <--> Leche              --  9
--         , A <--> Noruego            -- 10
--         , enseguida Guitarra Zorro  -- 11
--         , enseguida Caballo Bateria -- 12
--         , Violin <--> Naranjada     -- 13
--         , Japones <--> Teclado      -- 14
--         , enseguida Noruego Azul    -- 15
--         , constr16                  -- 16
--         , enseguida Perro Leche     -- 17
--         ]
st_2  = Ingles   <==> Roja
st_3  = Espanol  <==> Perro
st_6  = Verde    <==> Marfil |?> aDerechaDe
st_9  = C        <==> Leche
st_11 = Guitarra <==> Zorro  |?> enseguida


type CondFunc1 v = Maybe v -> Maybe v -> Bool

enseguida :: CondFunc1 ID
Just x `enseguida` Just y  = succ x == y || pred x == y

aDerechaDe :: CondFunc1 ID
Just x `aDerechaDe` Just y = succ y == x

aIzquierdaDe = flip aDerechaDe




