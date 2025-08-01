-- 1 --
-- a --
data Catedra = CatedraActiva {
    nombre :: String,
    filosofia :: String,
    profes :: Number,
    ayudantes :: Number,
    temas :: [String]
} | CatedraDadaDeBaja {
    nombre :: String,
    filosofia :: String,
    aniosDadaDeBaja :: Number
} deriving (Show, Eq)

data Facultad = Facultad {
    diasTurnoCompleto :: Number,
    diasMedioTurno :: Number,
    historialCatedras :: [Catedra]
} deriving (Show, Eq)

-- b --
paradigmasProgramacion :: Catedra
paradigmasProgramacion = CatedraActiva {
    nombre = "Paradigmas de Programación",
    filosofia = filosofiaFreire,
    profes = 13,
    ayudantes = 52,
    temas = temasParadigmas
}
  where
    filosofiaFreire = "El estudio no se mide por el número de páginas leídas en una noche, ni por la cantidad de libros leidos en un semestre. Estudiar no es un acto de consumir ideas, sino de crearlas y recrearlas"
    temasParadigmas = ["expresividad", "funcional", "lógico", "objetos", "declaratividad"]

-- 2 --
-- a --
ataqueBiologico :: Number -> Catedra -> Catedra
ataqueBiologico porcentaje catedra@(CatedraActiva nombre filosofia profes ayudantes temas) = 
    catedra { 
        profes = max 0 (profes - 2),
        ayudantes = max 0 (ayudantes - ayudantesInfectados)
    }
  where
    ayudantesInfectados = ayudantes * porcentaje / 100
ataqueBiologico _ catedra = catedra

-- b --
ataqueIdeologico :: [Char] -> Catedra -> Catedra
ataqueIdeologico letras catedra@(CatedraActiva nombre filosofia profes ayudantes temas) = 
    catedra {
        filosofia = filter (`notElem` letras) filosofia,
        temas = sacarUltimo temas
    }
  where
    sacarUltimo [] = []
    sacarUltimo xs = init xs
ataqueIdeologico _ catedra = catedra

-- c --
refuerzoAyudantistico :: Number -> Catedra -> Catedra
refuerzoAyudantistico nuevos catedra@(CatedraActiva {}) = 
    catedra { ayudantes = ayudantes catedra + nuevos }
refuerzoAyudantistico _ catedra = catedra

-- d --
resucitar :: Catedra -> Catedra -> Catedra
resucitar (CatedraDadaDeBaja nombreViejo filosofiaVieja _) catedra@(CatedraActiva {}) = 
    catedra { nombre = nombreViejo, filosofia = filosofiaVieja }
resucitar _ catedra = catedra

-- 3 --
aplicarEvento :: (Catedra -> Catedra) -> Catedra -> Catedra
aplicarEvento evento catedra@(CatedraActiva {}) = evento catedra
aplicarEvento _ catedra = catedra

-- 4 --
puedeEnsienar :: Facultad -> Catedra -> Bool
puedeEnsienar facultad (CatedraActiva _ filosofia profes _ temas) = 
    all id [profesesSuficientes, filosofiaAdecuada, temasSuficientes]
  where
    totalCursos = diasTurnoCompleto facultad * 3 + diasMedioTurno facultad * 2
    profesesSuficientes = profes * 2 >= totalCursos
    filosofiaAdecuada = length (words filosofia) >= 10 && length filosofia > 30
    temasSuficientes = length temas >= 2
puedeEnsienar _ (CatedraDadaDeBaja {}) = False

-- 5 --
-- a --
aplicarEventoEnFacultad :: (Catedra -> Catedra) -> Catedra -> Facultad -> Catedra
aplicarEventoEnFacultad evento catedra facultad
    | puedeEnsienar facultad catedraAfectada = catedraAfectada
    | otherwise = darDeBaja catedraAfectada
  where
    catedraAfectada = aplicarEvento evento catedra
    darDeBaja (CatedraActiva nombre filosofia _ _ _) = 
        CatedraDadaDeBaja nombre filosofia 0
    darDeBaja catedra = catedra

-- b --
pasarAnio :: [Catedra -> Catedra] -> Catedra -> Facultad -> Catedra
pasarAnio eventos catedra facultad = 
    foldl (\c evento -> aplicarEventoEnFacultad evento c facultad) 
          (envejecerCatedra catedra) 
          eventos
  where
    envejecerCatedra (CatedraActiva n f p a t) = 
        CatedraActiva n f (p + 1) (a + a * 0.1) t
    envejecerCatedra (CatedraDadaDeBaja n f anios) = 
        CatedraDadaDeBaja n f (anios + 1)

-- c --
pasarAnios :: [(Number, [Catedra -> Catedra])] -> Facultad -> Facultad
pasarAnios aniosEventos facultad = 
    facultad { historialCatedras = nuevasCatedras }
  where
    nuevasCatedras = map procesarCatedra (historialCatedras facultad)
    procesarCatedra catedra = foldl aplicarAnio catedra aniosEventos
    aplicarAnio catedra (_, eventos) = pasarAnio eventos catedra facultad

-- 6 --
-- plisquilmi a b c = map (b . snd c) . filter (> a)
-- Tipo inferido: (Ord t, Functor f) => t -> (u -> v) -> (w, x -> f t) -> f t -> f v
{--
Explicación:
plisquilmi :: (Ord t, Functor f) => t -> (u -> v) -> (w, x -> f t) -> f t -> f v
a :: t debe ser Ord para filter (> a)
b :: u -> v es función de transformación
c :: (w, x -> f t) es tupla con función que produce f t
Resultado: f t -> f v (función que transforma contenedor)

--}