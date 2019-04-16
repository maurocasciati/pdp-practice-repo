-- Clase 26/03/19

data Cliente = Cliente {
    nombre :: String,
    deuda :: Float
} deriving (Show)

clientes = [
    Cliente "Pedro" 6000,
    Cliente "Carlos" 500,
    Cliente "Juan" 10 ]

-- Saber que clientes deben mas de X monto (Solucion con Recursividad) (Baja Cohesion)

clientesQueDeben:: Float -> [Cliente] -> [Cliente]
clientesQueDeben monto [] = []
clientesQueDeben monto (cliente:clientes) 
    | deuda cliente > monto = cliente:clientesQueDeben monto clientes 
    | otherwise = clientesQueDeben monto clientes

--En Consola
--Main> clientesQueDeben 500 clientes
--[Cliente {nombre = "Pedro", deuda = 6000.0}]

-- Saber que clientes tienen el nombre X (Solucion con Recursividad) (Baja Cohesion)

clientesLlamado:: String -> [Cliente] -> [Cliente]
clientesLlamado nombreCompara [] = []
clientesLlamado nombreCompara (cliente:clientes) 
    | nombre cliente == nombreCompara = cliente:clientesLlamado nombreCompara clientes 
    | otherwise = clientesLlamado nombreCompara clientes

--En Consola
--Main> clientesLlamado "Juan" clientes
--[Cliente {nombre = "Juan", deuda = 10.0}]

-- Dividimos el problema en dos

-- deudaMayor devuelve True si la deuda del cliente es mayor a un monto X 

deudaMayorA:: Float -> Cliente -> Bool    
deudaMayorA monto cliente = deuda cliente > monto

-- nombreIgual devuelve True si el nombre de un Cliente es igual al nombre X

nombreIgual:: String -> Cliente -> Bool
nombreIgual nombreCompara cliente = nombre cliente == nombreCompara

-- cumpleCon se encarga de filtrar un lista de clientes por medio de un criterio (Ese criterio es una funcion que recibe un Cliente y devuelve un Bool)

cumplenCon:: (Cliente -> Bool) -> [Cliente] -> [Cliente] 
cumplenCon criterio clientes = filter criterio clientes

--En consola 
--Main> cumplenCon (nombreIgual "Juan") clientes
--[Cliente {nombre = "Juan", deuda = 10.0}]

--En consola 
--Main> cumplenCon (deudaMayorA 500) clientes
--[Cliente {nombre = "Pedro", deuda = 6000.0}]

-- Otras funciones de Orden Superior

-- Map 
-- Se encarga de transformar un lista de un cierto tipo en otro 

inicial:: Cliente -> Char
inicial cliente = (head.nombre) cliente

primeraLetra::(Cliente -> Char) -> [Cliente] -> [Char]
primeraLetra transformador clientes = map transformador clientes

-- En Consola
--Main> primeraLetra inicial clientes
--"PCJ" -> Transfomo la lista de clientes en una lista de las iniciales de los mismos

--All
-- Se encarga de devolver True si todos los elementos de la lista cumplen cierto criterio (Similar a "Para todo" matematico)

todosDebenMasDe:: Float -> [Cliente] -> Bool
todosDebenMasDe deuda clientes = all (clienteDebePlata deuda) clientes
 
-- En Consola
--Main> todosDebenMasDe 1000 clientes
--False

--Any
-- Se encarga de devolver True si al menos uno de los elementos de la lista cumple cierto criterio (Similar a "Existe" matematico)

algunoDebeMasDe:: Float -> [Cliente] -> Bool
algunoDebeMasDe deuda clientes = any (clienteDebePlata deuda) clientes

-- En consola
--Main> algunoDebeMasDe 1000 clientes
--True

-- Foldl / foldr
-- Se encarga de combinar los elementos de una lista usando una funcion de dos parametros (+, -, *, /)
-- Recibe como parametro una funcion binaria (de dos parametros), una semilla (comunmente el valor neutro de la funcion binaria) y una lista

-- En Consola
-- *Main> foldl (-) 0 [1,2,4]
-- -7

-- En Consola
-- *Main> foldr (-) 0 [1,2,4]
-- 3

-- Foldl y Foldr se diferencian en el orden en que se apliquen los parametros a la funcion binaria