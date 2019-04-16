-- 1.   Modelar a los animales: escribir un sinónimo de tipo y definir algunos ejemplos de animales como constantes. 
--      De un animal se sabe su coeficiente intelectual, su especie y sus capacidades. 
data Animal = Animal {
    coeficiente :: Integer,
    especie :: String,
    capacidades :: [String]
} deriving (Show)

pinky = Animal 5 "Raton" ["Molestar"]
cerebro = Animal 100 "Raton" ["Hacer planes", "Pensar"]
dumbo = Animal 40 "Elefante" ["Volar"]

-- 2.   Transformar a un animal de laboratorio:
-- inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual
inteligenciaSuperior :: Animal -> Integer -> Animal
inteligenciaSuperior animal aumento = animal { coeficiente = aumento + coeficiente animal } 

-- pinkificar: le quita todas las habilidades que tenía
pinkificar :: Animal -> Animal
pinkificar animal = animal { capacidades = [] }

-- superpoderes:    En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
--                  En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la habilidad de “hablar”. 
--                  Si no, lo deja como está. 
esEspecie :: Animal -> String -> Bool
esEspecie animal _especie = especie animal == _especie

esRatonInteligente :: Animal -> Bool
esRatonInteligente animal = (esEspecie animal "Raton") && (coeficiente animal >= 100) 

superpoderes :: Animal -> Animal
superpoderes animal | esEspecie animal "Elefante" = animal { capacidades = "No tenerle miedo a los ratones" : capacidades animal }
    | esRatonInteligente animal = animal { capacidades = "Hablar" : capacidades animal }
    | otherwise = animal



