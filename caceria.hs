--Caceria Monstruosa


-- Los Personajes

data Personaje = Personaje {
    experiencia :: Float,
    fuerzaBase :: Float,
    elemento :: Elemento
}

type Elemento = Float -> Float 

capacidadDeCaza :: Personaje -> Float 
capacidadDeCaza personaje = elemento personaje (fuerzaBase personaje)

calcularNivel :: Personaje -> Int
calcularNivel personaje = ceiling(((experiencia personaje) * (experiencia personaje)) / ((experiencia personaje) + 1))

--Otra Variante
-- calcularNivel (Personaje experiencia _ _) = ceiling(((experiencia) * (experiencia)) / ((experiencia + 1))

--Alquimistas

type Alquimista = Personaje -> Personaje

alquimistaAprendiz :: Personaje -> Personaje
alquimistaAprendiz personaje = personaje {elemento = (*2).elemento personaje}

alquimistaMaestro :: Float -> Personaje -> Personaje 
alquimistaMaestro anios personaje = personaje {elemento = ((*) ((+)((*) 0.1 anios) 1.0)).(elemento (alquimistaAprendiz personaje))}

alquimistaEstafador :: Personaje -> Personaje 
alquimistaEstafador personaje = personaje {elemento = (*1)}

superioresA :: Float -> Personaje -> [Alquimista] -> [Alquimista]
superioresA unValor personaje alquimistas = filter (compararCapacidad personaje unValor) alquimistas

compararCapacidad :: Personaje -> Float -> Alquimista -> Bool
compararCapacidad personaje unValor alquimista = (capacidadDeCaza (alquimista personaje)) > unValor

todosLeConvienen :: Personaje -> [Alquimista] -> Bool
todosLeConvienen personaje alquimistas = all (leConviene personaje) alquimistas

leConviene :: Personaje -> Alquimista -> Bool
leConviene personaje alquimista = compararCapacidad personaje (capacidadDeCaza personaje) alquimista

--Monstruos

data Habilidad = Habilidad {
    descripcion :: String, 
    tipo :: String
}

data Monstruo = Monstruo {
    especie :: String,
    resistencia :: Float,
    habilidades :: [Habilidad]
}

esAgresivo :: Monstruo -> Bool
esAgresivo unMonstruo = masHabilidades unMonstruo && resistenciaMayorA 0.0 unMonstruo && noEs "Chocobo" unMonstruo && noEs "Animal" unMonstruo

masHabilidades :: Monstruo -> Bool
masHabilidades unMonstruo = (length (filter esOfensiva (habilidades unMonstruo))) > (length (filter (not.esOfensiva )(habilidades unMonstruo))) 

esOfensiva :: Habilidad -> Bool
esOfensiva habilidad = (tipo habilidad) == "Magica" || (tipo habilidad) == "Fisica"

resistenciaMayorA :: Float -> Monstruo -> Bool
resistenciaMayorA unValor unMonstruo = (resistencia unMonstruo) > unValor

noEs :: String -> Monstruo -> Bool
noEs unaEspecie unMonstruo = (especie unMonstruo) /= unaEspecie

-- A la Caza!!

leGanaA :: Personaje -> Monstruo -> Bool
leGanaA personaje unMonstruo = not (resistenciaMayorA (capacidadDeCaza personaje) unMonstruo)  

type Mapa = [Monstruo]

recorrerMapa :: Personaje -> Mapa -> Personaje
recorrerMapa personaje mapa = foldl pelearCon personaje mapa

pelearCon :: Personaje -> Monstruo -> Personaje
pelearCon personaje monstruo 
    | leGanaA personaje monstruo = personaje {experiencia = experiencia personaje + 100.0}
    | otherwise = personaje {experiencia = experiencia personaje - 50, elemento = elemento personaje.(*0.9)}


hayAlgunoRecurriendoSinVencer :: Personaje -> Mapa -> Alquimista -> Bool
hayAlgunoRecurriendoSinVencer personaje mapa alquimista = any (not.leGanaA (alquimista personaje)) mapa


