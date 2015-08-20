{-# LANGUAGE MultiParamTypeClasses
            , TypeSynonymInstances
            , FlexibleInstances
            , FlexibleContexts
--            , ExistentialQuantification
--            , ImplicitParams
          #-}

module Example1 (
  main
) where

import Problem

import Data.List (intercalate)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data ID = A | B | C | D | E
        deriving (Eq, Enum, Show, Read)

data Color = Roja | Verde | Azul | Marfil | Amarilla
           deriving (Eq, Show)

data Nacionalidad = Ingles | Espanol | Ruso | Noruego | Japones
                  deriving (Eq, Show)

data Animal = Perro | Caracoles | Zorro | Caballo | Cebra
            deriving (Eq, Show)

data Bebida = Cafe | Te | Leche | Naranjada | Agua
            deriving (Eq, Show)

data Instrumento = Piano | Bateria | Guitarra | Teclado | Violin
                 deriving (Eq, Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class Value v var val where
    val :: v -> (var, val)
--    valVar :: v -> var
--    valVal :: v -> val
--
--    valVar v = fst (val v)

varID     = Var "ID"
varColor  = Var "Color"
varNacion = Var "Nacionalidad"
varAnimal = Var "Animal"
varBebida = Var "Bebida"
varMusic  = Var "Instrumento"


instance Value ID Var String where
    val a = (varID, show a)

instance Value Color Var String where
    val a = (varColor, show a)

instance Value Nacionalidad Var String where
    val a = (varNacion, show a)

instance Value Animal Var String where
    val a = (varAnimal, show a)

instance Value Bebida Var String where
    val a = (varBebida, show a)

instance Value Instrumento Var String where
    val a = (varMusic, show a)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Var = Var String deriving (Eq, Show)
data Id  = Id String  deriving (Eq, Show)

(<-->) :: (Value a Var String, Value b Var String) => a -> b -> Statement Var String
a <--> b = State (val a) (val b)

type Hechos = [Statement Var String]

--esVicino = mirror (||) f
--    where f x y = pred x == y || succ x == y

-- TODO:  use `mirror`

esVicino x y = pred x == y || succ x == y


enseguida v1 v2 = Cond3 var1 var2 varID f
               where (var1, val1) = val v1
                     (var2, val2) = val v2
                     f (v1, p1, _) (_, p2, v2) =
                        let pos1 = read p1 :: ID
                            pos2 = read p2
                        in val1 == v1 && val2 == v2 && esVicino pos1 pos2

cond6 = Cond2 varColor varID f
    where f ("Verde", pos1) ("Marfil", pos2) = succ marfilPos == verdePos
                                            where marfilPos = read pos2 :: ID
                                                  verdePos  = read pos1

constr16 = Constraint2 varNacion varID f
        where f ("Japones", p) = let pos = read p :: ID
                                 in pos == A || pos == E

hechos :: Hechos
hechos = [ Ingles <--> Roja          --  2
         , Espanol <--> Perro        --  3
         , Verde <--> Cafe           --  4
         , Ruso <--> Te              --  5
         , cond6                     --  6
         , Piano <--> Caracoles      --  7
         , Amarilla <--> Bateria     --  8
         , C <--> Leche              --  9
         , A <--> Noruego            -- 10
         , enseguida Guitarra Zorro  -- 11
         , enseguida Caballo Bateria -- 12
         , Violin <--> Naranjada     -- 13
         , Japones <--> Teclado      -- 14
         , enseguida Noruego Azul    -- 15
         , constr16                  -- 16
         , enseguida Perro Leche     -- 17
         ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO()
main = putStrLn $ intercalate "\n" (map show hechos)
