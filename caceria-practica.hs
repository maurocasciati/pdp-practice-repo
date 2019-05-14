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

type Alchemist = AlchemistType -> PJ -> PJ
type AlchemistType = Element -> Element

typeAprentice el = 2*el

typeMaster years el = typeAprentice ((*).(*).(/) el years ((1.1*).el))

typeSwindler = id

typeOther = (\x -> x * 3)



