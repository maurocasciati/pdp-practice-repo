-- PinkyYCerebro
--1

type Capacidad = String

data Animal = Animal {
    especie :: String,
    ci :: Int,
    capacidades :: [Capacidad]
 }

--2
type Transformacion = Animal -> Animal

inteligenciaSuperior :: Int -> Transformacion
inteligenciaSuperior cantidad animal  = animal {ci = ci animal + cantidad}

pinkificar :: Transformacion
pinkificar animal = animal {capacidades = []}

esEspecie :: Animal -> String -> Bool
esEspecie animal especieComp = especie animal == especieComp 

ciMayorA :: Int -> Animal -> Bool
ciMayorA cantidad animal = ci animal > cantidad

esRatonInteligente :: Animal -> Bool
esRatonInteligente animal  = esEspecie animal "Raton" && ciMayorA 100 animal

superPoderes :: Transformacion
superPoderes animal 
    | esEspecie animal "Elefante" = agregarHabilidad "No Tenerle Miedo a Los Ratones" animal
    | esRatonInteligente animal = agregarHabilidad "Hablar" animal
    | otherwise = animal

agregarHabilidad :: Capacidad -> Animal -> Animal    
agregarHabilidad capacidad animal  = animal {capacidades = capacidad : capacidades animal}

transformar :: Transformacion -> Animal -> Animal
transformar transformacion animal = transformacion animal

--3 

tieneHabilidad :: Animal -> Capacidad -> Bool 
tieneHabilidad animal capacidad =  capacidad `elem` (capacidades animal) 

type Criterio = Animal -> Bool

antropomorfico :: Criterio
antropomorfico animal = tieneHabilidad animal "Hablar" && ciMayorA 60 animal

noTanCuerdo :: Criterio 
noTanCuerdo animal = cantidadHabilidadesPinkiescas animal > 2

cantidadHabilidadesPinkiescas :: Animal -> Int
cantidadHabilidadesPinkiescas animal = length (capacidadesPinkiescas animal)

capacidadesPinkiescas :: Animal -> [Capacidad]
capacidadesPinkiescas animal = filter esPinkiesca (capacidades animal)

esPinkiesca :: Capacidad -> Bool
esPinkiesca capacidad = take 5 capacidad == "Pinki"
--esPinkiesca capacidad = zipWith  (==) "pinki" capacidad 

--4

data Experimento = Experimento {
    transformacion1 :: Transformacion,
    transformacion2 :: Transformacion,
    criterioExito :: Criterio
}

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso experimento animal = criterioExito experimento (realizarExperimento experimento animal)

realizarExperimento :: Experimento -> Animal -> Animal
realizarExperimento experimento animal = transformacion2 experimento (transformacion1 experimento animal)

--5 

listaCiAnimalesExitosos :: [Animal] -> Experimento -> [Int]
listaCiAnimalesExitosos animales experimento = map ci (animalesExitosos animales experimento)

animalesExitosos :: [Animal] -> Experimento -> [Animal]
animalesExitosos animales experimento = filter (experimentoExitoso experimento) animales

listaMitadExitosos :: [Animal] -> Experimento -> [Animal]
listaMitadExitosos animales experimento = take (mitadDe (animalesExitosos animales experimento)) (animalesExitosos animales experimento)

mitadDe :: [Animal] -> Int
mitadDe animales = length animales `div` 2 

listaCantidadCapacidadesRatonesExitosos :: [Animal] -> Experimento -> [Int] 
listaCantidadCapacidadesRatonesExitosos animales experimento = map cantidadCapacidades (ratonesExitosos animales experimento)

cantidadCapacidades :: Animal -> Int
cantidadCapacidades animal = length (capacidades animal)

ratonesExitosos :: [Animal] -> Experimento -> [Animal]
ratonesExitosos animales experimento = filter (esRatonExitoso experimento) animales

esRatonExitoso :: Experimento -> Animal -> Bool
esRatonExitoso experimento animal = experimentoExitoso experimento animal && esEspecie animal "Raton"
 
cerebro = Animal "Raton" 17 ["Destruir al mundo","Planes desalmados"]
experimentoCreado = Experimento pinkificar (inteligenciaSuperior 10) antropomorfico

--- Listas infinitas

animalInfinito = Animal 145 "Infinitosuario" (repeat "pinkiSoy")

experimentoDemo = Experimento [pinkificar, inteligenciaSuperior 10] antropomorfico
experimentoDemo2 = Experimento [ inteligenciaSuperior 90, superpoderes] antropomorfico
experimentoInfinito = Experimento [inteligenciaSuperior 15, superpoderes] superInteligencia
otroExperimentoInfinito = Experimento [inteligenciaSuperior 15, superpoderes] noTanCuerdo2