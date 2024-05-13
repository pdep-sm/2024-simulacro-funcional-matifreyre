module Library where
import PdePreludat

--1
{- Los autos se componen de marca, modelo, desgaste (ruedas y chasis, son dos
 números), velocidad máxima (m/s), y el tiempo de carrera-}
data Auto = Auto {
   marca :: String,
   modelo :: String,
   desgaste :: Desgaste,
   velocidadMaxima :: Number,
   tiempoDeCarrera :: Number
}

type Desgaste = (Number, Number)

desgasteChasis, desgasteRuedas :: Auto -> Number
desgasteChasis = fst . desgaste
desgasteRuedas = snd . desgaste

ferrari, lamborghini, fiat :: Auto
ferrari = Auto "Ferrari" "F50" (0,0) 65 0
lamborghini = Auto "Lamborghini" "Diablo" (7,4) 73 0
fiat = Auto {
   marca = "Fiat",
   modelo = "600",
   desgaste = (33,27),
   velocidadMaxima = 44,
   tiempoDeCarrera = 0
}

--2
estaEnBuenEstado, noDaMas :: Auto -> Bool
estaEnBuenEstado auto = desgasteChasis auto < 40 && desgasteRuedas auto < 60
noDaMas auto = desgasteChasis auto > 80 || desgasteRuedas auto > 80
noDaMas' :: Auto -> Bool
noDaMas' auto = (|| desgasteRuedas auto > 80) . (> 80) . desgasteChasis $ auto
noDaMas'' :: Auto -> Bool
noDaMas'' auto = any (> 80) [desgasteChasis auto, desgasteRuedas auto]
noDaMas''' :: Auto -> Bool
noDaMas''' auto = any ((> 80).($ auto)) [desgasteChasis, desgasteRuedas]

--3
reparar :: Auto -> Auto
reparar auto = auto { desgaste = (nuevoDesgasteChasis, 0) }
    where nuevoDesgasteChasis = desgasteChasis auto * 0.15

--4
type Tramo = Auto -> Auto

sumarTiempo :: Number -> Tramo
sumarTiempo tiempoAgregado auto = auto { tiempoDeCarrera = tiempoDeCarrera auto + tiempoAgregado } 
mapDesgasteChasis :: Number -> Tramo
mapDesgasteChasis nuevoDesgaste auto = auto { desgaste = (nuevoDesgaste, desgasteRuedas auto) } 
mapDesgasteRuedas :: Number -> Tramo
mapDesgasteRuedas nuevoDesgaste auto = auto { desgaste = (desgasteChasis auto, nuevoDesgaste) } 

--4.a
curva :: Number -> Number -> Tramo
{-
curva angulo longitud auto = auto {
    desgaste = (desgasteChasis auto, desgasteRuedas auto + 3 * longitud / angulo),
    tiempoDeCarrera = tiempoDeCarrera auto + longitud / ( velocidadMaxima auto / 2 )
}
-}
curva angulo longitud auto = 
    sumarTiempo (longitud / ( velocidadMaxima auto / 2 )) .
    mapDesgasteRuedas (desgasteRuedas auto + 3 * longitud / angulo) $ auto 

curvaPeligrosa, curvaTranca :: Tramo
curvaPeligrosa = curva 60 300
curvaTranca = curva 110 550

--4.b
recta :: Number -> Tramo
recta longitud auto = 
    sumarTiempo (longitud / velocidadMaxima auto) .
    mapDesgasteChasis (desgasteChasis auto + 0.01 * longitud) $ auto

tramoRectoClassic, tramito :: Tramo
tramoRectoClassic = recta 750
tramito = recta 280

--4.c
boxes :: Tramo -> Tramo
boxes tramo auto
    | estaEnBuenEstado auto = tramo auto
    | otherwise = 
        sumarTiempo (tiempoTramo tramo auto) . sumarTiempo 10 . reparar $ auto

tiempoTramo :: Tramo -> Auto -> Number
tiempoTramo tramo auto = tiempoDeCarrera (tramo auto) - tiempoDeCarrera auto 

--4.d
mojado :: Tramo -> Tramo
mojado tramo auto = sumarTiempo (tiempoTramo tramo auto / 2 ) . tramo $ auto 

--4.e 
ripio :: Tramo -> Tramo
ripio tramo = tramo . tramo 

--4.f
obstruccion :: Number -> Tramo -> Tramo
obstruccion metros tramo auto = 
    mapDesgasteRuedas (desgasteRuedas auto + 2 * metros) . tramo $ auto

--5 TBD

--6
superPista :: [Tramo]
superPista = [
    tramoRectoClassic,
    curvaTranca,
    mojado tramito,
    tramito,
    obstruccion 2 $ curva 80 400,
    curva 115 650,
    recta 970,
    curvaPeligrosa,
    ripio tramito,
    boxes $ recta 800 
    ]

myPrecious :: [(a1, b -> Number)] -> ((a1, b -> Number) -> [a2], b) -> Number
myPrecious ts x = 
    sum . map (($ snd x) . snd) . filter ((>5) . length . fst x) $ ts


