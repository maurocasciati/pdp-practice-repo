type Persona = (String,String, Int,[Float])

juanJoven::Persona
juanJoven = ("juan","joven", 5, [1,2])

juanViejo::Persona
juanViejo = ("juan","viejo", 50, [19,223,435])

esJoven:: Persona -> Bool
esJoven alguien = edad alguien < 45

edad (_, _, e, _ ) = e 
gastos (_, _, _, g ) = g

gastoMasQue :: Float -> Persona -> Bool
gastoMasQue cantidad alguien = totalGastos alguien > cantidad

impuesto:: Persona -> Float
impuesto alguien 
 | esJoven alguien = totalGastos alguien * 0.21
 | otherwise = 0

totalGastos::Persona->Float
totalGastos alguien = sum (gastos alguien)

cumplirAnios::Persona -> Persona
cumplirAnios (nombre, ape, edad, gastos) = (nombre, ape, edad + 1 ,gastos)

comprarseAlgo:: Float -> Persona -> Persona
comprarseAlgo precio (nombre, ape, edad, gastos) = (nombre, ape, edad, precio:gastos)

cumpleaniosCompleto:: Float -> Persona -> Persona
--cumpleaniosCompleto precio (nombre, edad, gastos) = (nombre, edad + 1, precio:gastos)
cumpleaniosCompleto precio persona = cumplirAnios ( comprarseAlgo precio persona)

--cumpleaniosCompleto   persona = (cumplirAnios. comprarseAlgo precio) persona
