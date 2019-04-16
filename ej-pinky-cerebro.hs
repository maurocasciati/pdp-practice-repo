import Data.List

main :: IO()
main = print("Ejercicio Pinky y Cerebro")

-- 1.   Modelar a los animales: escribir un sinónimo de tipo y definir algunos ejemplos de animales como constantes. 
--      De un animal se sabe su coeficiente intelectual, su especie y sus capacidades. 
type Especie = String
type Capacidad = String
type Transformacion = Animal -> Animal
type Criterio = Animal -> Bool

data Animal = Animal {
    coeficiente :: Integer,
    especie :: Especie,
    capacidades :: [Capacidad]
} deriving (Show)

pinky = Animal 5 "Raton" ["PinkiMolestar","PinkiHablar","Caminar"]
cerebro = Animal 100 "Raton" ["Hacer planes", "Pensar"]
dumbo = Animal 40 "Elefante" ["Volar"]

-- 2.   Transformar a un animal de laboratorio:
-- inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual
inteligenciaSuperior :: Animal -> Integer -> Animal
inteligenciaSuperior animal aumento = animal { coeficiente = aumento + coeficiente animal } 

-- pinkificar: le quita todas las habilidades que tenía
pinkificar :: Transformacion
pinkificar animal = animal { capacidades = [] }

-- superpoderes:    En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
--                  En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la habilidad de “hablar”. 
--                  Si no, lo deja como está. 
esEspecie :: Especie -> Criterio
esEspecie _especie animal = especie animal == _especie

tieneCoeficienteMayorA :: Integer -> Criterio
tieneCoeficienteMayorA num animal = coeficiente animal >= num

esRatonInteligente :: Criterio
esRatonInteligente animal = (esEspecie "Raton" animal) && (tieneCoeficienteMayorA 100 animal) 

agregarCapacidad :: Capacidad -> Animal -> Animal 
agregarCapacidad cap animal = animal { capacidades = cap : capacidades animal }

superpoderes :: Transformacion
superpoderes animal 
    | esEspecie "Elefante" animal = agregarCapacidad "No tenerle miedo a los ratones" animal 
    | esRatonInteligente animal = agregarCapacidad "Hablar" animal
    | otherwise = animal

-- 3.   Desarrollar los siguientes criterios:
-- antropomorfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60.
tieneCapacidad :: Capacidad -> Criterio
tieneCapacidad capacidad animal = elem capacidad $ capacidades animal

antropomorfico :: Criterio
antropomorfico animal = (tieneCapacidad "Hablar" animal) && (tieneCoeficienteMayorA 60 animal)

-- noTanCuerdo: si tiene más de dos habilidades pinkiescas, es decir, que empiece con "pinki". 
esCapacidadPinkiesca :: Capacidad -> Bool
esCapacidadPinkiesca = isPrefixOf "Pinki" --needs import Data.List
--esCapacidadPinkiesca = zipWith (==) "Pinki"

capacidadesPinkiescas :: [Capacidad] -> [Capacidad]
capacidadesPinkiescas = filter esCapacidadPinkiesca

noTanCuerdo :: Criterio
noTanCuerdo animal = length (capacidadesPinkiescas $ capacidades animal) >= 2

-- 4. 

data Experimento = Experimento {
  transformacion1 :: Transformacion,
  transformacion2 :: Transformacion,
  criterioExito :: Criterio
}


