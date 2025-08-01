-- SaPHO --
cambiarElemento posicion elemento lista =  take (posicion - 1) lista ++ [ elemento ] ++ drop posicion lista


data Edificio = Edificio {
    pisos :: [Piso],
    valorBaseM2 :: Number,
    coefRobustez :: Number -- Entre 0 y 1
} deriving (Show, Eq)


data Piso = Piso {
    departamentos :: [Departamento]
} deriving (Show, Eq)

data Departamento = Departamento {
    superficie :: Number,
    habitabilidad :: Number --(entre 0 y 100)
} deriving (Show, Eq)

-- 2 --
-- a--
cheto :: Edificio -> Bool
cheto (Edificio pisos _ _) =   all unPisoUnDepto pisos
    where unPisoUnDepto piso = (==1).length.departamentos $ piso

-- b --
pajarera :: Edificio -> Bool
pajarera (Edificio pisos _ _) =   all minimo6Pisos pisos
    where minimo6Pisos piso = (>=6).length.departamentos $ piso

-- c --
piramide :: Edificio -> Bool
piramide (Edificio pisos _ _) = 
    all (\(actual, siguiente) -> actual > siguiente) $ zip cantidadesDepts (tail cantidadesDepts)
    where cantidadesDepts = map (length.departamentos) pisos

--3 --
precioDeptoMasCar :: Edificio -> Number
precioDeptoMasCar edificio = 5 

--precio depto = superficio depto + valorBase edificio * coefRobustez edificio
--3 --
precioDeptoMasCaro :: Edificio -> Number
precioDeptoMasCaro edificio = 
    maximum . map (maximum . map (precio edificio) . departamentos) $ pisos edificio
    where precio edif depto = superficie depto * valorBaseM2 edif * coefRobustez edif

-- 4 -- 
-- a --
merge :: [Departamento] -> Departamento
merge deptos =  foldl1 mergeDeptos deptos
    where 
    mergeDeptos d1 d2 = Departamento { 
        superficie = superficie d1 + superficie d2,
        habitabilidad = max 0 (min 100 (habitabilidad d1 + habitabilidad d2) / 2)
    }
-- b --
split ::Number -> Departamento -> [Departamento]
split n departamento = map (divideEnNIguales n) $ replicate n departamento

divideEnNIguales n depto = depto{
    superficie = superficie depto / n,
    habitabilidad = max 0 (min 100 (habitabilidad depto / n)) 
}

-- 5 --
-- a --
incendio :: Number -> Edificio -> Edificio
incendio pisoDanio edificio = edificio {
    pisos = map afectarPiso (zip [1..] (pisos edificio)),
    coefRobustez = coefRobustez edificio / 2
  }
  where
    afectarPiso (numeroPiso, piso)
      | numeroPiso >= pisoDanio = piso { departamentos = map reducirHabitabilidad (departamentos piso) }
      | otherwise = piso
    reducirHabitabilidad depto = depto { 
        habitabilidad = max 0 (habitabilidad depto - 30) 
    }
-- b --
plaga :: Number -> Number -> Edificio -> Edificio
plaga numeroPiso puntos edificio = edificio {
    pisos = map afectarSiEsPiso (zip [1..] (pisos edificio))
  }
  where
    afectarSiEsPiso (num, piso)
      | num == numeroPiso = piso { departamentos = map reducirHabitabilidad (departamentos piso) }
      | otherwise = piso
    reducirHabitabilidad depto = depto { 
        habitabilidad = max 0 (habitabilidad depto - puntos) 
    }

-- c --
terremoto :: Number -> Edificio -> Edificio
terremoto reduccion edificio = edificio {
    coefRobustez = max 0 (coefRobustez edificio - reduccion)
  }

-- 6 --
-- a --
ampliacion :: Number -> Number -> Edificio -> Edificio
ampliacion cantDeptos metrosTotales edificio = edificio {
    pisos = pisos edificio ++ [pisoNuevo]
  }
  where
    pisoNuevo = Piso { departamentos = replicate (round cantDeptos) deptoUnitario }
    deptoUnitario = Departamento {
        superficie = metrosTotales / cantDeptos,
        habitabilidad = 100
    }

-- b --
fumigacion :: Edificio -> Edificio
fumigacion edificio = edificio {
    pisos = map mejorarPiso (pisos edificio)
  }
  where
    mejorarPiso piso = piso { departamentos = map mejorarDepto (departamentos piso) }
    mejorarDepto depto
      | habitabilidad depto < 60 = depto { habitabilidad = min 100 (habitabilidad depto + 20) }
      | otherwise = depto

-- c --
mergeEdificio :: Number -> Edificio -> Edificio
mergeEdificio numeroPiso edificio = edificio {
    pisos = cambiarElemento (round numeroPiso) pisoMergeado (pisos edificio)
  }
  where
    pisoMergeado = Piso { departamentos = [merge (departamentos pisoOriginal)] }
    pisoOriginal = (pisos edificio) !! (round numeroPiso - 1)

-- d --
splitEdificio :: Number -> Number -> Edificio -> Edificio
splitEdificio cantNuevos numeroPiso edificio = edificio {
    pisos = cambiarElemento (round numeroPiso) pisoSpliteado (pisos edificio)
  }
  where
    pisoSpliteado = Piso { departamentos = departamentosAnteriores ++ departamentosNuevos }
    pisoOriginal = (pisos edificio) !! (round numeroPiso - 1)
    departamentosAnteriores = init (departamentos pisoOriginal)
    ultimoDepto = last (departamentos pisoOriginal)
    departamentosNuevos = split cantNuevos ultimoDepto

{--
7.	Dada la siguiente función, determine y explique su tipo:

funcionLoca a b c = 
	all ((>c) . fst a) . foldl (\x y -> b y . snd a $ x) [] 

a es una tupla: tiene una función fst a y una función snd a

foldl empieza con [] y recibe un acumulador x y un valor y

b y . snd a $ x → entonces snd a es una función que transforma x y b y la aplica

Después se hace all ((>c) . fst a) sobre el resultado del foldl

funcionLoca 
  :: (d -> e, d -> f)  -- la tupla `a` con funciones
  -> (g -> f -> f)     -- `b`, una función que transforma `f` en función del input
  -> e                 -- `c`, valor con el que se compara
  -> [g]               -- una lista de inputs
  -> Bool

--}