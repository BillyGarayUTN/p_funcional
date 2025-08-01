-- Parcial BonAppetit

type Comida = Persona -> Persona
type Gramos = Number
-- 1 Punto A
data Persona = Persona {
    calorias :: Number,
    nutrientes :: [String]
} deriving (Show, Eq)

-- 1 Punto B
agregarNutriente :: String -> Persona -> Persona
agregarNutriente nuevoNutriente persona
    | nuevoNutriente `elem` nutrientes persona = persona
    | otherwise   = persona { nutrientes = nuevoNutriente : nutrientes persona }

-- 2 

-- Punto A
tomate :: Persona -> Persona
tomate = agregarNutriente "vitamina C" .agregarNutriente "vitamina A"

-- Punto B
zanahoria :: Persona -> Persona
zanahoria = tomate . agregarNutriente "vitamina E" . agregarNutriente "vitamina K"

-- Punto C
carne :: Gramos -> Persona -> Persona
carne gramos persona =
    agregarNutriente "calcio" . agregarNutriente "hierro" $ persona {calorias = (24 * gramos) + calorias persona}

-- Punto D
pan :: String -> Persona -> Persona
pan tipo persona
    | tipo == "blanco"   = agregarNutriente "zinc" $ persona { calorias = calorias persona + 265 }
    | tipo == "integral" = agregarNutriente "zinc" . agregarNutriente "fibra" $ persona { calorias = calorias persona + 200 }
    | tipo == "papa"     = agregarNutriente "zinc" $ persona { calorias =(caloriasPapa+) $ calorias persona }
    | otherwise          = persona
    where caloriasPapa
            | calorias persona > 2000 = 100
            | otherwise               = 500

-- Punto E
hamburguesaCheta :: Persona -> Persona
hamburguesaCheta = pan "papa" . carne 180 . tomate . pan "papa"
    --foldr ($) persona [pan "papa",carne 180 ,tomate , pan "papa"]

-- Punto 3 
menu :: [Comida] -> Persona -> Persona
menu comidas persona = foldr ($) persona comidas

comidas :: [Persona -> Persona]
comidas = [hamburguesaCheta,zanahoria,pan "integral"]
sofia = Persona {
    calorias = 0,
    nutrientes = []
}

-- Punto 4
data Evento = Evento{
    nombreEvento :: String,
    menuEvento :: [Comida],
    invitados :: [Persona]
}

altaFiesta :: Evento -> Bool
altaFiesta (Evento _ _ invitados)= all pipona invitados

pipona :: Persona -> Bool
pipona persona = (calorias persona > 2000) || (length (nutrientes persona) > 5)

-- punto 5
colarseEnEventos :: Persona -> [Evento] -> [Evento]
colarseEnEventos persona = map (colarseSiAltaFiesta persona)

colarseSiAltaFiesta :: Persona -> Evento -> Evento
colarseSiAltaFiesta persona evento
    | altaFiesta evento = evento { invitados = persona : invitados evento }
    | otherwise         = evento

-- Punto 6
{--

Justificar conceptualmente: ¿Se podría determinar si un evento es alta
fiesta si tuviese infinitos invitados?

-- Respuesta --
La función all con evaluación lazy puede devolver False rápidamente si 
encuentra al primer invitado que no es pipona, sin recorrer toda la lista.

--}