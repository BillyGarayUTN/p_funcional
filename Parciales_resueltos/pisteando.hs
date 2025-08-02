-- Punto 1 --
-- Modelado del Auto
data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgaste :: (Number, Number), -- (ruedas, chasis)
    velocidadMax :: Number,
    tiempoCarrera :: Number -- Tiempo acumulado en la carrera
} deriving (Show, Eq)

-- Ejemplos de autos
autoFerrari :: Auto
autoFerrari = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgaste = (0, 0), -- (ruedas, chasis)
    velocidadMax = 65,
    tiempoCarrera = 0
}

autoLamborghini :: Auto
autoLamborghini = Auto {
    marca = "Lamborghini",
    modelo = "Diablo",
    desgaste = (4, 7), -- (ruedas, chasis)
    velocidadMax = 73,
    tiempoCarrera = 0
}

autoFiat :: Auto
autoFiat = Auto {
    marca = "Fiat",
    modelo = "600",
    desgaste = (27, 33), -- (ruedas, chasis)
    velocidadMax = 44,
    tiempoCarrera = 0
}

-- Aux !!
desgasteRuedas :: Auto -> Number
desgasteRuedas auto = fst (desgaste auto)

desgasteChasis :: Auto -> Number
desgasteChasis auto = snd (desgaste auto)

-- Punto 2 --
--  a --
buenEstado :: Auto -> Bool
buenEstado (Auto  _ _ (ruedas,chasis) _ _) = ruedas < 60 && chasis < 40


noDaMas :: Auto -> Bool
noDaMas (Auto  _ _ (ruedas,chasis) _ _) = ruedas > 80 || chasis > 80

-- Punto 3 --
-- a --

reparar :: Auto -> Auto
reparar auto = auto {desgaste = (0, nuevoChasis) }
    where nuevoChasis =(*0.15).desgasteChasis $ auto

-- Punto 4 --
-- a --
curva :: Number -> Number -> Auto -> Auto
curva angulo longitud auto = auto {
    desgaste = (desgasteRuedas auto + desgastePorCurva, desgasteChasis auto),
    tiempoCarrera = tiempoCarrera auto + tiempoExtra
}
    where
    desgastePorCurva = 3 * longitud / angulo
    tiempoExtra = longitud / (velocidadMax auto / 2)

-- b --
tramoRecto :: Number -> Auto -> Auto
tramoRecto longitud auto = auto {
    desgaste = (desgasteRuedas auto, desgasteChasis auto + (longitud / 100)),
    tiempoCarrera = tiempoCarrera auto + (longitud / velocidadMax auto)
}

tramoRectoClassic :: Auto -> Auto
tramoRectoClassic = tramoRecto 750

tramito :: Auto -> Auto
tramito = tramoRecto 280

-- c --
boxes :: (Auto -> Auto) -> Auto -> Auto
boxes tramo auto
    | buenEstado auto = tramo auto
    | otherwise = (reparar . sumarPenalidad . tramo) auto
    where
        sumarPenalidad a = a { tiempoCarrera = tiempoCarrera a + 10 }

-- d -- 
mojado :: (Auto -> Auto) -> Auto -> Auto
mojado tramo auto = auto {
    desgaste = desgaste (tramo auto),
    tiempoCarrera = tiempoCarrera (tramo auto) + ((tiempoCarrera (tramo auto) - tiempoCarrera auto) / 2)
}


--  e -- !!
ripio :: (Auto -> Auto) -> Auto -> Auto
ripio tramo = (duplicarTiempo . duplicarDesgaste) . tramo
    where
        duplicarTiempo auto = auto { tiempoCarrera = tiempoCarrera auto + (tiempoCarrera auto - tiempoCarrera (tramo auto)) }
        duplicarDesgaste auto = auto {
            desgaste = (
                2 * desgasteRuedas auto - desgasteRuedas (tramo auto),
                2 * desgasteChasis auto - desgasteChasis (tramo auto)
            )
        }

-- f --
obstruccion :: Number -> (Auto -> Auto) -> Auto -> Auto
obstruccion metros tramo auto = autoFinal {
    desgaste = (desgasteRuedas autoFinal + (2 * metros), desgasteChasis autoFinal)
}
    where
        autoFinal = tramo auto



-- Punto 5 -- 
pasarPorTramo :: (Auto -> Auto) -> Auto -> Auto
pasarPorTramo tramo auto
    | noDaMas auto = auto
    | otherwise = tramo auto

--punto 6 --
curvaTranca = curva 110 550
curvaPeligrosa = curva 60 300
curvaConObstruccion = obstruccion 2 (curva 80 400)
curvaNormal = curva 115 650
tramoRecto970 = tramoRecto 970
boxes800 = boxes (tramoRecto 800)
mojadoTramito = mojado tramito
ripioTramito = ripio tramito

superPista :: [Auto -> Auto]
superPista = [
    tramoRectoClassic,
    curvaTranca,
    mojadoTramito,
    tramito,
    curvaConObstruccion,
    curvaNormal,
    tramoRecto970,
    curvaPeligrosa,
    ripioTramito,
    boxes800]

-- FORMA 1: Recursión simple y clara
peganLaVuelta :: [Auto -> Auto] -> [Auto] -> [Auto]
peganLaVuelta pista = map (aplicarTramos pista)
  where
    aplicarTramos [] auto = auto
    aplicarTramos (tramo:tramos) auto = aplicarTramos tramos (pasarPorTramo tramo auto)

-- FORMA 2: Con foldl más directo
-- peganLaVuelta pista autos = map (\auto -> foldl (\a tramo -> pasarPorTramo tramo a) auto pista) autos

-- FORMA 3: Point-free con composición
-- peganLaVuelta pista = map (foldl (.) id (map pasarPorTramo pista))


-- 7 a  b --
type Carrera = ([Auto -> Auto], Number)

tourBuenosAires :: Carrera
tourBuenosAires = (superPista, 20)


-- c -- 
correrCarrera :: Carrera -> [Auto] -> [[Auto]]
correrCarrera (_, 0) autos = [autos]
correrCarrera (pista, vueltas) autos =
    autos : correrCarrera (pista, vueltas - 1) (filter (not . noDaMas) autosPostVuelta)
    where autosPostVuelta = peganLaVuelta pista autos

