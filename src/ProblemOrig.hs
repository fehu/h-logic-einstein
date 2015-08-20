{-# LANGUAGE MultiParamTypeClasses
            , ExistentialQuantification
            , ImplicitParams
          #-}

module ProblemOrig (
  main
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data ID = A | B | C | D | E
        deriving (Eq, Enum, Show)

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


data Casa m = Casa { casaId :: ID
                   , color  :: m Color
                   , nacion :: m Nacionalidad
                   , animal :: m Animal
                   , bebida :: m Bebida
                   , musica :: m Instrumento
                   }

-- TODO explicit types restrictions
es          = declare
viveEnCasa  = es
tiene       = es
bebe        = es
toca        = es

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type Cond a = a -> a -> Bool
type Constraint a = a -> Bool


data Declaration d = Declare d
                   | Cond (Cond d)
                   | Constraint (Constraint d)

declare a b = Declare (a, b)

(<-->) = declare


instance (Show d) => Show (Declaration d) where
    show (Declare a)     = "Declare " ++ show a
    show (Cond  _)       = "Condition*"
    show (Constraint  _) = "Constraint*"

class AssignDeclared m a where
    assign :: Casa m -> a -> Casa m

-- stub for compatibility
instance AssignDeclared Maybe ID where
    assign (Casa id c n a b m) _ = Casa id c n a b m

instance AssignDeclared Maybe Color where
    assign (Casa id _ n a b m) c = Casa id (return c) n a b m

instance AssignDeclared Maybe Nacionalidad where
    assign (Casa id c _ a b m) n = Casa id c (return n) a b m

instance AssignDeclared Maybe Animal where
    assign (Casa id c n _ b m) a = Casa id c n (return a) b m

instance AssignDeclared Maybe Bebida where
    assign (Casa id c n a _ m) b = Casa id c n a (return b) m

instance AssignDeclared Maybe Instrumento where
    assign (Casa id c n a b _) m = Casa id c n a b (return m)

instance (AssignDeclared m a, AssignDeclared m b) => AssignDeclared m (a, b) where
    assign c (x, y) = assign (assign c x) y

instance (AssignDeclared m a, AssignDeclared m b, AssignDeclared m c) =>
    AssignDeclared m (a, b, c) where
    assign c (x, y, z) =  assign (assign (assign c x) y) z

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class (Show v) => Variable v where
    varName :: v -> String

compareVar :: (Variable a, Variable b) => a -> b -> Bool
compareVar x y = varName x == varName y


instance Variable ID where varName _ = "ID"
instance Variable Color where varName _ = "Color"
instance Variable Nacionalidad where varName _ = "Nacionalidad"
instance Variable Animal where varName _ = "Animal"
instance Variable Bebida where varName _ = "Bebida"
instance Variable Instrumento where varName _ = "Instrumento"

instance (Variable a, Variable b) => Variable (a,b)
instance (Variable a, Variable b, Variable c) => Variable (a,b,c)

instance (Variable d) => Variable (Declaration d)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data AssignableDeclaration m = forall d. (Show d, Variable d, AssignDeclared m d)
                             => Declarar (Declaration d)

instance Show (AssignableDeclaration Maybe) where
    show (Declarar a) = show a

instance Variable (AssignableDeclaration Maybe) where
    varName (Declarar a) = varName a

type Hechos = [AssignableDeclaration Maybe]

infixr 0 `que`
que = ($)

mirror :: (b -> b -> b) -> (a -> a -> b)  -> a -> a -> b
mirror g f p1 p2 = let c1 = f p1 p2
                       c2 = f p2 p1
                 in g c1 c2

cond6 :: Cond (Color, ID)
cond6 = mirror (||) cond6'

cond6' (Verde, verdePos) (Marfil, marfilPos) = succ marfilPos == verdePos
cond6' _ _ = False

--cond11 :: Cond3 Instrumento ID Animal
--cond11 = mirror (||) cond11'

esVicino = mirror (||) f
    where f x y = pred x == y || succ x == y

enseguida :: (Eq a, Eq b) => a -> b -> Declaration (a, ID, b)
enseguida a b = Cond $ mirror (||) f
    where f (a', pos1, _) (_, pos2, b') = a == a'
                                       && b == b'
                                       && esVicino pos1 pos2

--cond11' (Guitarra, pos1, _) (_, pos2, Zorro) = esVicino pos1 pos2
--cond11' _ _ = False

--cond12 :: Cond3 Animal ID Instrumento
--cond12 = mirror (||) cond12'

--cond12' (Caballo, pos1, _) (_, pos2, Bateria) = esVicino pos1 pos2
--cond12' _ _ = False

constr16 (Japones, A) = True
constr16 (Japones, E) = True
constr16 _ = False

hechos :: Hechos
hechos = [ Declarar `que` Ingles `viveEnCasa` Roja  --  2
         , Declarar `que` Espanol `tiene` Perro     --  3
         , Declarar `que` Verde <--> Cafe           --  4
         , Declarar `que` Ruso `bebe` Te            --  5
         , Declarar `que` Cond cond6                --  6
         , Declarar `que` Piano <--> Caracoles      --  7
         , Declarar `que` Amarilla <--> Bateria     --  8
         , Declarar `que` C <--> Leche              --  9
         , Declarar `que` A <--> Noruego            -- 10
         , Declarar `que` enseguida Guitarra Zorro  -- 11
         , Declarar `que` enseguida Caballo Bateria -- 12
         , Declarar `que` Violin <--> Naranjada     -- 13
         , Declarar `que` Japones `toca` Teclado    -- 14
         , Declarar `que` enseguida Noruego Azul    -- 15
         , Declarar `que` Constraint constr16       -- 16
         , Declarar `que` enseguida Perro Leche     -- 17
         ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- f = Try to apply the declarations that depend on _ :: [applied]; apply simpler first
--
-- g = Apply f recursively to it's results until an empty list is returned
--
--
--

f :: Hechos -> Casa Maybe -> (Hechos, Casa Maybe)
f hs casa = let x = filter (compareVar A) hs
            in undefined

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO()
main = print hechos


