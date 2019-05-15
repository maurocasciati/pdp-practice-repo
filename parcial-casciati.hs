{-  ENUNCIADO:

A- Potrero:

El objetivo es averiguar quien gana en un torneo de barrio.
De cada jugador queremos saber: nombre, puntaje (que tan bien juega).

Un equipo (de varios jugadores) le gana a otro si su puntaje es mayor al de otro equipo.
El puntaje de un equipo es el promedio del puntaje de sus primeros 3 jugadores.

1- Hacer una funcion que determine quien gana entre dos equipos
2- Los jugadores a veces se juntan antes del partido a tomar algo, eso mejora el rendimiento de equipo: 
    Mejora el puntaje de cada jugador porcentualmente segun lo que toman: cafe 10%, agua 0%, somnifero -5% (agregar mas)
3- Quien gana el torneo: Determinar el equipo que gana, que es el que gane todos los partidos.


B- Libertadores:

En la libertadores hay jugadores y equipos, pero tambien hay tecnicos.
Entonces, el puntaje de cada equipo entonces depende del estilo del tecnico, que puede ser:
    - Promedio de los 5 primeros jugadores,
    - Puntaje del mejor jugador,
    - Estilo a eleccion...

1- Determinar quien gana entre dos equipos.
2- Ayudin: el equipo que recibe ayudin mejora el rendimiento del tecnico, que hace jugar mejor al equipo:
    - Si recibe, mejora un 10% el puntaje del equipo
3- El equipo que gane el torneo es el que gane todas las llaves de eliminacion, arrancando desde octavos.

C- ¿Qué pasa si un equipo de Potrero quiere jugar en la libertadores? ¿Y al revés?

D- ¿Qué pasa si hay una lista infinita de equipos?

-}

import Text.Show.Functions
import Data.List

data Jugador = Jugador {
    nombre :: String,
    puntaje :: Float
} deriving (Show)

data Equipo = Equipo {
    nombreEquipo :: String,
    jugadores :: [Jugador]
} deriving (Show)

instance Eq Jugador where
    j1 == j2 = (nombre j1) == (nombre j2)

instance Ord Jugador where
    j1 <= j2 = (puntaje j1) <= (puntaje j2)

j1 = Jugador "Juan Gabriel" 6
j2 = Jugador "Jose Jugador" 7
j3 = Jugador "Otro Jugador" 8
j4 = Jugador "Juana Jugadora" 7
j5 = Jugador "Otra Jugadora" 5
j6 = Jugador "Pau Lina" 10
j7 = Jugador "Gabriel Juan" 3
j8 = Jugador "Caro Pardiaco" 10
j9 = Jugador "Don Vicente" 9

e1 = Equipo "Kampeones" [j1, j2, j3]
e2 = Equipo "Las capas" [j4, j5, j6]
e3 = Equipo "Visitantes" [j7, j8, j9]


--A1: Quien gana un partido de potrero:

puntajeEquipoPotrero :: Equipo -> Float
puntajeEquipoPotrero equipo = promedioEquipo 3.0 equipo

promedioEquipo :: Float -> Equipo -> Float
promedioEquipo total equipo = (sum (map puntaje (jugadores equipo))) / total

pierdeMejorJugador :: Equipo -> Equipo
pierdeMejorJugador equipo = equipo { jugadores = sacarMejorJugador (jugadores equipo) }

sacarMejorJugador :: [Jugador] -> [Jugador]
sacarMejorJugador jugadores = delete (mejorJugador jugadores) jugadores

mejorJugador :: [Jugador] -> Jugador
mejorJugador jugadores = maximum jugadores

quienGanaPotrero :: Equipo -> Equipo -> Equipo
quienGanaPotrero eq1 eq2 
    | puntajeEquipoPotrero eq1 >= puntajeEquipoPotrero eq2 = pierdeMejorJugador eq1
    | otherwise = pierdeMejorJugador eq2


--A2: Cambiar puntaje jugadores:

previaDelPartido :: String -> Equipo -> Equipo
previaDelPartido bebida equipo = equipo { jugadores = cambiarRendimientoJugadores bebida (jugadores equipo) }

cambiarRendimientoJugadores :: String-> [Jugador] -> [Jugador]
cambiarRendimientoJugadores bebida jugadores = map (tomarBebida bebida) jugadores

tomarBebida :: String -> Jugador -> Jugador
tomarBebida bebida jugador 
    | bebida == "agua" = jugador 
    | bebida == "cafe" = jugador { puntaje = (puntaje jugador) * 1.1}
    | bebida == "somnifero" = jugador { puntaje = (puntaje jugador) * 0.95}
    | bebida == "birrita" = jugador { puntaje = (puntaje jugador) * 1.2}


--A3: Quien gana un torneo:
quienGanaTorneoPotrero :: [Equipo] -> Equipo
quienGanaTorneoPotrero equipos = foldl quienGanaPotrero (head equipos) equipos 


--PARTE B: LIBERTADORES:

data EquipoLibertadores = EquipoLibertadores {
        nombreEquipoLibertadores :: String,
        jugadoresEquipoLibertadores :: [Jugador],
        tecnico :: EstiloTecnico
} deriving (Show)

type EstiloTecnico = EquipoLibertadores -> Float
estiloDelPotrero equipo = promedioEquipoLibertadores 7.0 equipo
estiloElitista equipo = puntaje (mejorJugador (jugadoresEquipoLibertadores equipo))
estiloIncentivador equipo = promedioEquipoLibertadores 5.0 (previaDelPartidoLibertadores "cafe" equipo)

j11 = Jugador "nombre" 9.2
j12 = Jugador "nombre" 10
j13 = Jugador "nombre" 8
j14 = Jugador "nombre" 7.7
j15 = Jugador "nombre" 8.1
j16 = Jugador "nombre" 9.6
j17 = Jugador "nombre" 8.4
j18 = Jugador "nombre" 8.6
j19 = Jugador "nombre" 9.3
j20 = Jugador "nombre" 7.6
j21 = Jugador "nombre" 9.2
j22 = Jugador "nombre" 8.6
j23 = Jugador "nombre" 7.9
j24 = Jugador "nombre" 9.3
j25 = Jugador "nombre" 8.6

el1 = EquipoLibertadores "Kampeones Libertadores" [j1, j2, j3, j11, j12, j13, j14, j15] estiloDelPotrero
el2 = EquipoLibertadores "Las capas Libertadores" [j4, j5, j6, j16, j17, j18, j19, j20] estiloElitista
el3 = EquipoLibertadores "Visitantes Libertadores" [j7, j8, j9, j21, j22, j23, j24, j25] estiloIncentivador


--B1: Quien gana partido de libertadores:

puntajeEquipoLibertadores :: EquipoLibertadores -> Float
puntajeEquipoLibertadores equipo = (tecnico equipo) equipo

promedioEquipoLibertadores :: Float -> EquipoLibertadores -> Float
promedioEquipoLibertadores total equipo = (sum (map puntaje (jugadoresEquipoLibertadores equipo))) / total

previaDelPartidoLibertadores :: String -> EquipoLibertadores -> EquipoLibertadores
previaDelPartidoLibertadores bebida equipo = equipo { jugadoresEquipoLibertadores = cambiarRendimientoJugadores bebida (jugadoresEquipoLibertadores equipo) }

quienGanaPartidoLibertadores :: EquipoLibertadores -> EquipoLibertadores -> EquipoLibertadores
quienGanaPartidoLibertadores eq1 eq2 
    | puntajeEquipoLibertadores eq1 >= puntajeEquipoLibertadores eq2 = pierdeMejorJugadorEquipoLibertadores eq1
    | otherwise = pierdeMejorJugadorEquipoLibertadores eq2

pierdeMejorJugadorEquipoLibertadores :: EquipoLibertadores -> EquipoLibertadores
pierdeMejorJugadorEquipoLibertadores equipo = equipo { jugadoresEquipoLibertadores = sacarMejorJugador (jugadoresEquipoLibertadores equipo) }

--B2: Ayudin:

recibirAyudin :: EquipoLibertadores -> EquipoLibertadores
recibirAyudin equipo = equipo { tecnico = estiloConAyudin (tecnico equipo) }

estiloConAyudin :: EstiloTecnico -> EstiloTecnico
estiloConAyudin estilo = 1.1 * estilo

--B3: Campeon libertadores:

-- quienGanaTorneoLibertadores :: [Equipo] -> Equipo
-- quienGanaTorneoLibertadores equipos