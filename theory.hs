-- DATA SIMPLE:

data Pirata = Pirata {
    nombrePirata :: String,
    botin :: [Tesoro]
} deriving (Show)

-- DATA COMPUESTO:

data Tesoro = 
  UnTesoro {
      nombreTesoro :: String,
      valor :: Double }|
  BonoDefault {
    cotizaciones :: [Double] }|
  LeLiq {
    importeNominal :: Double,
    pais :: Pais  } deriving Show

class InstanciaTesoro a where
  nombreDeTesoro :: a -> String
  valorTesoro :: a -> Double

instance InstanciaTesoro Tesoro where
  nombreDeTesoro (UnTesoro nombre _) = nombre
  nombreDeTesoro (BonoDefault _) = "Bono"
  nombreDeTesoro (LeLiq _ pais) = "Leliq" ++ " " ++ (nombrePais pais)
  valorTesoro (UnTesoro _ valor) = valor
  valorTesoro (BonoDefault cotizaciones) = valorDeCotizaciones cotizaciones
  valorTesoro (LeLiq imp pais) = valorDeLeLiq imp pais

-- INSTANCIAR ORDEN Y EQUIVALENTE DE UN DATA:
instance Eq Tesoro where 
    b1 == b2 = nombreDeTesoro b1 == nombreDeTesoro b2 && (valorTesoro b1) == (valorTesoro b2)
  
instance Ord Tesoro where
    b1 <= b2 = (valorTesoro b1) <= (valorTesoro b2)
  
-- DATA PARA DISTINTOS TIPOS

data TipoUniversidad = AntiDictaminante | BuitreAlternativa | AtlanticaInofensiva

class UniversidadPirata a where
  desarrollarSaqueo :: a -> Barco -> Barco

instance UniversidadPirata TipoUniversidad where
  desarrollarSaqueo AntiDictaminante barco = barco {  formaDeSaquear = not.formaDeSaquear barco}
  desarrollarSaqueo BuitreAlternativa barco = barco { formaDeSaquear = (tesoroEsSaqueable ([(formaDeSaquear barco)] ++ [saqueoBuitre, tesoroEsValioso]))}
  desarrollarSaqueo AtlanticaInofensiva barco = barco

-- MANEJO DE LISTAS:

-- ++: agrega item a lista
botin = tesoros ++ botin

--MAP: aplica todas las variables de la lista a una funcion
nombreFuncionMap listaVariables funcion = map funcion listaVariables  

--ANY: devuelve bool si una lista cumple al menos una vez una condicion
nombreFuncionAny listaVariables condicion = any condicion listaVariables

--MAXIMUM: devuelve el maximo de la lista (se puede cambiar derivando Org en un data)
nombreFuncionMaximum listaVariables = maximum listaVariables

--FOLDL FOLDR: aplica la var a una funcion con todos los elem de la lista
nombreFuncionFoldl funcion var lista = foldl funcion var lista
aplicarFuncionFoldr var listaFunciones = foldr ($) var listaFunciones

--WIPWITH: aplica la funcion a los parametros de dos listas (uno a uno en orden)
nombreFuncionZipWith funcion lista1 lista2 = zipWith funcion lista1 lista2

-- LISTAS INFINITAS: [1..] o directamente (repeat var)

-- EXPRESION LAMBDA:
invertirConLambda = (\a b -> b a)
sumarUnoConLambda = (\x -> x + 1)

