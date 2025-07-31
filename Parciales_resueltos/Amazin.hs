
--  PARCICAL  AMAZIN  -- RESUELTO
type Tipo = String
-- Parte A --

data Persona = Persona {
    nick :: String,
    felicidad :: Number,
    libroAdquirido :: [Libro],
    librosLeidos :: [Libro]
} deriving (Show, Eq)

billy = Persona {
    nick = "Billy",
    felicidad = 100,
    libroAdquirido = [libro1],
    librosLeidos = [libro2]
}

data Libro = Libro {
    titulo :: String,
    autor :: String,
    paginas :: Number,
    genero :: Persona-> Persona
} deriving (Show, Eq)

libro1 = Libro {
    titulo = "El Principito",
    autor = "Antoine de Saint-Exupéry",
    paginas = 96,
    genero = terror
}
libro2 = Libro {
    titulo = "1984",
    autor = "George Orwell",
    paginas = 328,
    genero = comedia "accion"
}

-- Parte B --
-- generos --

comedia :: Tipo -> Persona -> Persona
comedia tipo persona
    | "dramaticas" == tipo = persona
    | "absurdas" == tipo = persona {felicidad = (+5).felicidad $ persona }
    | "satiricas" == tipo = persona {felicidad = (*2).felicidad $ persona}
    | otherwise = persona {felicidad = (+10).felicidad $ persona}

ficcion :: Persona -> Persona
ficcion persona = persona {nick = reverse.nick $ persona}

terror :: Persona -> Persona
terror persona = persona {libroAdquirido = eliminarTodos . libroAdquirido $ persona}
    where eliminarTodos _ = []

-- Parte C --
leeLibro :: Libro -> Persona -> Persona
leeLibro libro persona = genero libro $ persona{librosLeidos = libro : librosLeidos persona}

sePoneAlDia:: Persona -> Persona
sePoneAlDia persona = foldr leeLibro persona librosParaLeer
    where librosParaLeer = filter (not . flip yaFueLeido (librosLeidos persona)) (libroAdquirido persona)

-- Inicio Auxiliares sePoneAlDia --
mismoLibro :: Libro -> Libro -> Bool
mismoLibro libro1 libro2 = titulo libro1 == titulo libro2 && autor libro1 == autor libro2

yaFueLeido :: Libro -> [Libro] -> Bool
yaFueLeido libro librosLeidos = any (mismoLibro libro) librosLeidos
-- Fin Auxiliares sePoneAlDia --

fanatico :: String -> Persona -> Bool
fanatico escritorEstrella (Persona _ _ _ librosLeidos) = all (sonDelMismoAutor escritorEstrella) librosLeidos

sonDelMismoAutor :: String -> Libro -> Bool
sonDelMismoAutor unAutor libro = unAutor == (autor libro)

{--
●	¿Puede una persona ponerse al día si adquirió una cantidad infinita de libros? Justificar.
No, una persona no puede ponerse al día si adquirió una cantidad infinita de libros.

El problema surge en dos puntos:

El filtrado (filter): Para determinar qué libros no fueron leídos, filter debe recorrer toda la lista de libros adquiridos.
Si esta lista es infinita, el filtrado nunca termina.

El foldr: Aunque foldr en Haskell es perezoso y podría comenzar a procesar elementos antes de que termine el filtrado, 
en este caso necesita que filter complete su trabajo para saber cuáles son los libros a leer.

En resumen:

La evaluación quedaría "colgada" en el paso del filtrado y nunca llegaría a leer ningún libro.

--}

-- Parte D --
amazin :: Libro -> String
amazin libro
    | paginas libro < 100 = "cuento"
    | paginas libro <= 200 && paginas libro >= 100 = "novela corta"
    | otherwise = "novela"

titulosAdquiridosPorTipo :: Persona -> String -> [String]
titulosAdquiridosPorTipo persona tipoLibro = map titulo librosDelTipo
    where librosDelTipo = filter (\libro -> amazin libro == tipoLibro) (libroAdquirido persona)