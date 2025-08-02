-- gnomos --
data Material = Material {
    nombre :: String,
    calidad :: Number
} deriving (Show, Eq)

data Edificio = Edificio {
    tipoEdificio :: String,
    materiales :: [Material]
} deriving (Show, Eq)

data Aldea = Aldea {
    poblacion :: Number,
    materialesDisponibles :: [Material],
    edificios :: [Edificio]
} deriving (Show, Eq)

-- punto 1 --
-- a --
esValioso :: Material -> Bool
esValioso = (>= 50).calidad

-- b --
disponibles :: String -> Aldea -> Number
disponibles unMaterial aldea = length . filter ((== unMaterial) . nombre) $ materialesDisponibles aldea

-- c --
valorTotal :: Aldea -> Number
valorTotal aldea =
    sum . map calidad . (concatMap materiales (edificios aldea)++) $ materialesDisponibles aldea

-- punto 2 --
-- a --
tenerGnomito :: Aldea -> Aldea
tenerGnomito aldea = Aldea {poblacion = (+1).poblacion $ aldea}

-- b --
lustrarMaderas :: Aldea -> Aldea
lustrarMaderas aldea = aldea {materialesDisponibles = map lustrarSiEsMadera (materialesDisponibles aldea)}
    where
    lustrarSiEsMadera material
        | esMadera material = material {calidad = min 100 (calidad material + 5)}
        | otherwise = material
    esMadera material =  "Madera" == nombre material

-- c --
recolectar :: Material -> Number -> Aldea -> Aldea
recolectar nuevoMaterial cantidad aldea = aldea {
    materialesDisponibles =(++ replicate cantidad nuevoMaterial) $ materialesDisponibles aldea
}

-- punto 3 --
-- a --
edificiosChetos ::Aldea -> [Edificio]
edificiosChetos aldea = filter (any esValioso . materiales) $ edificios aldea

-- b --
materialesComunes :: Aldea -> [String]
materialesComunes aldea
    | null (edificios aldea) = []
    | otherwise = filter estaEnTodos nombresDelPrimero
    where
    todosLosNombres = map (map nombre . materiales) (edificios aldea)
    nombresDelPrimero = map nombre . materiales . head $ edificios aldea
    estaEnTodos nombreMaterial = all (elem nombreMaterial) todosLosNombres

-- punto 4 --
-- a --
type Tarea = Aldea -> Aldea
realizarLasQueCumplan :: [Tarea] -> (Aldea -> Bool) -> Aldea -> Aldea
realizarLasQueCumplan tareas condicion aldea = foldl realizarTarea aldea tareas
    where
    realizarTarea aldea tarea
        | condicion (tarea aldea) = tarea aldea
        | otherwise = aldea
-- b --
-- i --
tenerGnomitosConComida :: Aldea -> Aldea
tenerGnomitosConComida aldea = realizarLasQueCumplan tareas condicionComida aldea
    where
    tareas = [tenerGnomito, tenerGnomito, tenerGnomito]
    condicionComida aldea = unidadesComida aldea > poblacion aldea
    unidadesComida = disponibles "Comida"

-- ii --
recolectarYLustrarMaderas :: Aldea -> Aldea
recolectarYLustrarMaderas aldea = realizarLasQueCumplan tareas condicionValiosos aldea
    where
    tareas = [recolectar maderaPino 30, lustrarMaderas]
    maderaPino = Material "Madera de pino" (calidadMaximaMaderas aldea)
    condicionValiosos aldea = all esValioso (materialesDisponibles aldea)
    calidadMaximaMaderas aldea = 
        maximum . map calidad . filter esMadera $ materialesDisponibles aldea
    esMadera material = take 6 (nombre material) == "Madera"
    