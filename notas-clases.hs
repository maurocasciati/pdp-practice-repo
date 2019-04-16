esVocal :: Char -> Bool
esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal x = False

f::Integer->Integer
f 0 = 1
f x = x * f(x-1)

signo::Long->String
signo 0 = "cero"
signo x | x>0 = "positivo"
		| otherwise = "negativo"
		
-- alguien gasta mas que x
type Persona = (String,Integer,[Integer])
juanJoven::Persona
juanJoven = ("Juan",30,[25,5,3])

gastoMasQue :: Persona -> Integer -> Bool
gastoMasQue (_, _, x) maximo | sum x > maximo = True
							 | otherwise = False
						   
-- alguien es joven <45

esJoven :: Persona -> Bool
esJoven (_, x, _) | x < 45 = True
                  | otherwise = False

-- cuanto paga alguien de impuesto (21% y mayores excentos)

calculoImpuesto :: Persona -> Double
calculoImpuesto (_, _, gastos) = fromIntegral(sum gastos) * 0.21

cuantoPagaImpuesto :: Persona -> Double

cuantoPagaImpuesto persona | esJoven persona = calculoImpuesto persona
    | otherwise = 0.0
	
-- cumplir años

cumplirAnios :: Persona -> Persona
cumplirAnios (nombre, x, gastos) = (nombre, x + 1, gastos)

-- cumplir años y comprarse algo

comprarseAlgo :: Persona -> Integer -> Persona
comprarseAlgo (nombre, x, gastos) nuevoGasto = (nombre, x, gastos:nuevoGasto)

cumplirAniosCompleto :: Persona -> Integer -> Persona
cumplirAniosCompleto persona nuevoGasto = (nombre, x + 1, gastos:nuevoGasto)

-- biografia

bajar haskero, haskell syntax y haskelly para vs code

------------ CLASE 26/3 ----------------

--definicion de tipos de datos:
data Persona = UnaPersona String Integer
data Fecha = Dia Integer Integer Integer

data Persona = UnaPersona {
		nombre :: String,
		edad :: Integer,
		nacminiento :: Fecha
}

----------Listas:
head \  tail
 _   :  _  _  _      --pensarlo como un gusano
    init     \last

--ejemplo de last con recursividad:
ultimo [x] = x
ultimo (x:cola) = ultimo cola 
--longitud con recursividad
longitud [] = 0 
longitud (x:cola) = 1 + longitud cola

-- ejercicio de ejemplo de juan pablo:
data Cliente = Cliente {
	nombre :: String,
	deuda :: Float
} deriving (Show) -- esto se usa para poder imprimir los tipos de datos definidos
-- para mostrar funciones: import Text.Show.Functions

clientes = [
	Cliente "Pedro" 6000,
	Cliente "Juan" 0,
	Cliente "Carlos" 500
]

clientesQueDeben :: Float -> [Cliente] -> [Cliente]

clientesQueDeben monto [] = []
clientesQueDeben monto (cliente:restoClientes)
		| deuda cliente > monto = cliente : clientesQueDeben monto restoClientes
		| otherwise = clientesQueDeben monto restoClientes
		
clientesLlamados :: String -> [Cliente] -> [Cliente]

clientesLlamados nombreBuscado [] = []
clientesLlamados nombreBuscado (cliente:restoClientes)
		| nombre cliente == nombreBuscado = cliente : clientesLlamados nombreBuscado restoClientes
		| otherwise = clientesLlamados nombreBuscado restoClientes
		
-- Funciones de orden superior: (Reciben funciones y listas, y aplican la funcion a cada elemento de la lista)
-- funciones de orden superior predefinidas: all - any - map - filter - foldl - foldr

deudaMayorA:: Float -> Cliente -> Bool
deudaMayorA monto cliente = deuda cliente > monto

nombreIgual:: String -> Cliente -> Bool
nombreIgual nombreBuscado cliente = nombre cliente == nombreBuscado

cumpleCon:: (Cliente -> Bool) -> [Cliente] -> [Cliente]
cumpleCon criterio clientes = filter criterio clientes --aplicacion parcial de los parametros
		-- en consola quedaria: cumpleCon (nombreIgual "Juan") clientes
		-- ejemplo map en consola: map deuda clientes (devuelve lista de deudas de clientes)

inicial:: Cliente -> Char
inicial cliente = (head.nombre) cliente

traerLetra:: (Cliente -> Char) -> [Cliente] -> [Char]
traerLetra transformador clientes = map transformador clientes
		--en consola: traerLetra inicial cliente (devuelve una lista de las letras iniciales de los clientes)

-- ejemplo composicion y aplicacion parcial
( map ((2*).(1+)).filter (>5) ) [1..10] --recibe la lista del 1 al 10 y a los >5 les hace el doble del siguiente

-- ejemplo lista de funciones
ejemplo = [ 1+, 2*, siguiente, max 5, f abs 3 ]
-- ejemplo tipo de dato con funciones
data Profe = UnProfe {
	nombre :: String
	calificacion :: String -> Integer
}

exigente examen = length examen/10
nilomiro examen = 6
facilongo examen | elem "x" examen = 10
				| otherwise = 8
				
calificar profe examen = (calificacion profe) examen 
		-- en consola: calificar (UnProfe "Juan" exigente) "Hola no se nada"    (devuelve 1.5 ya que length es 15 y /10 da 1.5)
