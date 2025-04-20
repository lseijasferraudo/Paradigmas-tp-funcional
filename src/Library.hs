module Library where
import PdePreludat
import GHC.Integer (Integer)

import Prelude -- lo agregue porque no me tomaba el Int

--doble :: Number -> Number
--doble numero = numero + numero

-- Defino los tipos de datos
type Marca = String
type Modelo = String
type Apodo = String
type Numero = Int
type DesgasteRuedas = Numero
type DesgasteChasis = Numero
type VelocidadMaxima = Numero
type TiempoEnCarrera = Numero
-- fin de los tipos de datos


-- defino el desgaste como un conjunto de datos
-- el desgaste de las ruedas es un entero y el desgaste del chasis es un entero

-- ver como hacer del desgaste un tipo de dato
--data Desgaste = Desgaste {
--    desgasteRuedas :: DesgasteRuedas,
--    desgasteChasis :: DesgasteChasis
--}

-- defino el auto como un conjunto de datos
-- el auto tiene una marca, un modelo, un apodo, una velocidad maxima, un tiempo en carrera y un desgaste
-- el desgaste es un conjunto de datos que tiene un desgaste de rueda y un desgaste de chasis
data Auto = Auto {
    marca :: Marca,
    modelo :: Modelo,
    apodo :: [Apodo],
    velocidadMaxima :: VelocidadMaxima,
    tiempoEnCarrera :: TiempoEnCarrera,
    desgasteRuedas :: Numero,
    desgasteChasis :: Numero
} --deriving (Show, Eq) -- Agrego Show y Eq para poder ver los autos y compararlos


-- Defino los autos con sus respectivos datos

-- Primer auto
f50 = Auto "Ferrari" "F50" ["La nave", "El fierro", "Ferrucho"] 65 0 0 0

-- Segundo auto
diablo = Auto "Lamborghini" "Diablo" ["Lambo", "La bestia"] 73 0 4 7

-- Tercer auto
fitito = Auto "Fiat" "600" ["La bocha", "La bolita", "Fitito"] 44 0 27 33

-- Cuarto auto
p504 = Auto "Peugeot" "504" ["El rey del desierto"] 40 0 0 0

-- Fin de los autos



-- punto 2, definir funcion de estado de salud del auto
-- Puntos a tener en cuenta:
-- 1. Los peugeot no estan JAMAS en buen estado (nadie sabe porque)
-- 2. si es de otra marca:
-- 2.1 Si tiene una canditad de tiempoEnCarrera < 100, se deternuma cuando el desgasteChasis < 20
-- 2.2 En caso contrario, cuando el desgasteChasis < 40 y desgasteRuedas < 60

-- Verifico si esta buen estado
estadoSalud :: Auto -> String
estadoSalud auto  
    | esPeugeot auto = "No está en buen estado"
    | tiempoEnCarrera auto < 100 && desgasteChasis auto < 20 = "Está en buen estado"
    | tiempoEnCarrera auto >= 100 && desgasteChasis auto < 40 && desgasteRuedas auto < 60 = "Está en buen estado"
    | otherwise = "No está en buen estado"

-- Ver como implementar de otra forma *to do*

-- funciones auxiliares para el punto 2
esPeugeot :: Auto -> Bool
esPeugeot auto = marca auto == "Peugeot"


-- fin punto 2



--b saber si un auto da para mas, 
-- desgasteChasis > 80 y el primer apodo empieza por "La "
-- desgasteRuedas > 80


