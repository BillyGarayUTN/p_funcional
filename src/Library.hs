module Library where
import PdePreludat

{-
doble :: Number -> Number
doble numero = numero + numero
-}



-- Empezamos a trabajar y resolver el tp de friends

-- type
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
viejoSonLosTrapos = all esVital . filter esJoven

esVital :: Persona -> Bool
esVital persona = nivelEnergia persona > 100

nivelTotalDeAnsiedad :: [Persona] -> Number
nivelTotalDeAnsiedad = sum . map ansiedad . filter esJoven

losMasCriticados :: (Persona -> Bool) -> [Persona] -> [String]
losMasCriticados criterio = map nombre . take 2 . filter criterio

-- punto 3
