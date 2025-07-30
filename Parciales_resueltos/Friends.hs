type Yulius = Number -- Energia
type Alegria = Number  -- alegronio
type Ansiedad = Number -- nerviofrina
type Tareas = [Persona -> Persona]

data Persona = Persona {
    nombre :: String,
    edad :: Number,
    alegria :: Number,
    ansiedad :: Number,
    tareas:: [Persona -> Persona]
} deriving (Show, Eq)

nivelEnergia :: Persona -> Number
nivelEnergia persona
    | alegria persona > ansiedad persona = min 340 (alegria persona * 2)
    | ansiedad persona > alegria persona && esJoven persona = subtract (stress persona) 300
    | otherwise = (+10) $ alegria persona

stress :: Persona -> Number
stress persona
    | (>5).length.tareas $ persona = (1.5*) $ ansiedad persona
    | otherwise = ansiedad persona

esJoven :: Persona -> Bool
esJoven persona = (<40) $ edad persona

-- punto 2

viejoSonLosTrapos :: [Persona] -> Bool
viejoSonLosTrapos = all esVital . filter (not.esJoven)

esVital :: Persona -> Bool
esVital persona = nivelEnergia persona > 100

nivelTotalDeAnsiedad :: [Persona] -> Number
nivelTotalDeAnsiedad = sum . map ansiedad . filter esJoven

losMasCriticados :: (Persona -> Bool) -> [Persona] -> [String]
losMasCriticados criterio = map nombre . take 2 . filter criterio

-- punto 3

tareaRealizada :: Persona -> Persona
tareaRealizada persona = persona{ansiedad = max 0 . subtract 10 . ansiedad $  persona}

codearUnProyectoNuevo :: Persona -> Persona
codearUnProyectoNuevo persona = tareaRealizada $ persona{alegria = (+110).alegria $ persona , ansiedad = (+50).ansiedad $ persona}

hacerTramitesEnAfip :: Number -> Persona -> Persona
hacerTramitesEnAfip nTramites persona = tareaRealizada $ persona{ ansiedad  = max 300 (ansiedad persona * nTramites) }

andarEnBici :: Number ->Persona -> Persona
andarEnBici km persona = tareaRealizada $ persona {ansiedad = 0, alegria = ((50*km)+).alegria $ persona}

escucharMusica :: Persona -> Persona
escucharMusica persona = tareaRealizada $ persona {ansiedad = subtract 10 . ansiedad $ persona}

-- punto  4

energiaResultante :: Persona -> Tareas -> Number
energiaResultante persona = nivelEnergia . foldr ($) persona

-- punto 5

hiceLoQuePude :: Persona -> Tareas -> Persona
hiceLoQuePude persona [] = persona
hiceLoQuePude persona (tarea:tareas)
    | nivelEnergia persona' > 100 = hiceLoQuePude persona' tareas
    | otherwise = persona'
    where persona' = tarea persona

-- punto 6

-- Dada una lista de personas infinitas,
-- ¿podemos determinar el nivelTotalDeAnsiedad o si viejosSonLosTrapos? 
-- Justifique su respuesta

{--
nivelTotalDeAnsiedad:
No se puede determinar para una lista infinita, porque la función necesita 
sumar la ansiedad de todos los jóvenes de la lista. Como la suma requiere 
recorrer toda la lista, nunca termina y el resultado no se puede obtener.

viejoSonLosTrapos:
En este caso, gracias a la evaluación perezosa, sí es posible obtener un 
resultado en algunos casos.

Si existe al menos un joven que no es vital en los primeros elementos, la 
función devolverá False sin recorrer toda la lista.
Si todos los jóvenes son vitales y la lista es infinita, la función nunca
terminará porque intentará chequear infinitos elementos.

Resumen:

nivelTotalDeAnsiedad no puede calcularse con listas infinitas.
viejoSonLosTrapos puede devolver resultado con listas infinitas
solo si encuentra un joven no vital rápidamente, 
gracias a la pereza de Haskell. Si no, tampoco termina.
--}