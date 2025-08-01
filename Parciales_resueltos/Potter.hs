--   Parcial de potter --  Resuelto
type Hechizo = Mago -> Mago

data Mago = Mago{
    nombre :: String,
    edad :: Number,
    salud :: Number,
    hechizos :: [Mago -> Mago]
} deriving (Show, Eq)

-- punto 1 --
-- a --
curar :: Number -> Mago -> Mago
curar vida mago = mago{ salud = (vida+).salud $ mago }

-- b --
lanzarRayo :: Mago -> Mago
lanzarRayo mago 
    | salud mago > 10 = mago{ salud = subtract 10 (salud mago) }
    | otherwise = mago{ salud = (salud mago) `div` 2 }

-- c --
amnesia :: Number -> Mago -> Mago
amnesia cantidad mago = mago { hechizos = drop cantidad (hechizos mago) }

-- d --
confundir :: Mago -> Mago
confundir mago = head (hechizos mago) $ mago
-- flip ($) mago ((head).hechizos $ mago)

-- punto 2 --
-- a --
poder :: Mago -> Number
poder (Mago _ edad salud hechizos) = ((edad * (length hechizos))+) $ salud

-- b --
danio :: Hechizo -> Mago -> Number
danio hechizo mago = salud mago - (salud . hechizo) mago

-- c --
diferencialDePoder :: Mago -> Mago -> Number
diferencialDePoder mago otroMago = abs $ (poder otroMago) - poder mago

-- punto 3 --
data Academia = Academia {
    magos :: [Mago],
    examenDeIngreso :: Mago -> Bool
}

-- a --
rincenwindSinHechizos :: Academia -> Bool
rincenwindSinHechizos (Academia magos _) = any esRincenwindSinHechizos magos
    where esRincenwindSinHechizos mago = esRincenwind mago && sinHechizos mago

esRincenwind :: Mago -> Bool
esRincenwind mago = "rincenwind" == nombre mago

sinHechizos :: Mago -> Bool
sinHechizos mago = null (hechizos mago)

-- b --
nionioYViejo :: Academia -> Bool
nionioYViejo (Academia magos _) = all esnionio . filter esViejo $ magos

esViejo :: Mago -> Bool
esViejo (Mago _ edad _ _) = edad > 50

esnionio :: Mago -> Bool
esnionio (Mago _ _ salud hechizos) =  (> salud).length $ hechizos

-- c --
hacerExamen :: Academia -> Number
hacerExamen (Academia magos examen) = length . filter (not.examen) $ magos

-- d --
sumEdadMagosCon10Hechizos :: Academia -> Number
sumEdadMagosCon10Hechizos academia = sum. map edad $ sabenMas10Hechizos academia

sabenMas10Hechizos :: Academia -> [Mago]
sabenMas10Hechizos (Academia magos _) = filter (sabeMas10) magos
    where sabeMas10 mago = length (hechizos mago) > 10


-- punto 4 --

maximoSegun criterio valor comparables = foldl1 (mayorSegun $ criterio valor) comparables

mayorSegun evaluador comparable1 comparable2
    | evaluador comparable1 >= evaluador comparable2 = comparable1
    | otherwise                                      = comparable2

mejorHechizoContra :: Mago -> Mago -> Hechizo
mejorHechizoContra mago otroMago = maximoSegun (flip danio) mago (hechizos otroMago)


mejorOponente :: Mago -> Academia -> Mago
mejorOponente mago (Academia magos _) = maximoSegun (flip diferencialDePoder) mago magos

-- 5 --
noPuedeGanarle :: Mago -> Mago -> Bool
noPuedeGanarle mago otroMago = mismaSalud mago $ foldr ($) mago (hechizos otroMago)

mismaSalud :: Mago -> Mago -> Bool
mismaSalud mago otroMago = salud mago == salud otroMago
