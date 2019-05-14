data PJ = PJ {
    name :: String,
    xp :: Float,
    strenght :: Float,
    element :: Element
}

type Element = Float -> Float

espadaOxidada = (1.2*)
katanaFilosa  = (10+).(0.9*)
sableLambdico cm = ((1+cm/100)*)
redParadigmatica = sqrt
baculoDuplicador x= x * 2
espadaMaldita = espadaOxidada.sableLambdico 89

marco = PJ "marco" 15 10 katanaFilosa
paul  = PJ "paul" 17 6 baculoDuplicador
lucas = PJ "lucas" 7 69 espadaMaldita

level :: PJ -> Integer
level pj = ceiling (((xp pj) * (xp pj)) / ((xp pj) + 1))

huntingLevel :: PJ -> Float
huntingLevel pj = (element pj) (strenght pj)

type Alchemist = PJ -> PJ

typeAprentice pj = pj { element = (2.0 * (element pj))}
typeMaster years pj = pj { element = typeAprentice ((1+(0.1*years)) * (element pj)) }
typeSwindler = id
typeOther = (\x -> x * 3)

danCapacidadCazaMayorA :: Float -> PJ -> [Alchemist] -> [Alchemist]
danCapacidadCazaMayorA capacidad pj alquimistas = filter (capacidadMayorA capacidad pj) alquimistas

capacidadMayorA :: Float -> PJ -> Alchemist -> Bool
capacidadMayorA capacidad pj alquimista = ((>capacidad).huntingLevel.alquimista) pj 
