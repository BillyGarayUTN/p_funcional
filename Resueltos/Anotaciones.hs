
-- Uso de Constreuctones 
data ColorPrimario = Rojo | Azul | Amarillo

data Alumno = Alumno {
    nombre :: String,
    fechaNacimiento :: (Number, Number, Number),
    legajo :: Number,
    materiasQueCursa :: [String]
    --criterioEstudio :: CriterioEstudio 
} deriving (Show,Eq)  -- Show para imprimir, Eq para comparación

recargoPorColor :: ColorPrimario -> String
recargoPorColor Rojo = "rojo"
recargoPorColor Azul = "azul"
recargoPorColor Amarillo = "amarillo"

billy = Alumno{nombre="Billy", fechaNacimiento = (27,09,1990), legajo=1570066 , materiasQueCursa= ["pdp","Fisica2"] }


saberNotas :: Alumno -> ColorPrimario-> String
saberNotas alumno color = recargoPorColor color
-- Uso: saberNotas billy Rojo  <-- Este Rojo es un constructor


-- uso construcotres multriples
data ColorPrimario = Rojo | Amarillo | Azul deriving (Show, Eq)

recargoPorColor :: ColorPrimario -> Number
recargoPorColor Rojo = 50
recargoPorColor _ = 20

data Camiseta = Camiseta {
    talle :: String,
    precioBase :: Number
} deriving (Show)

precioFinal :: Camiseta -> ColorPrimario -> Number
precioFinal camiseta color = precioBase camiseta + (recargoPorColor color)


peru = Camiseta{ talle="s" , precioBase = 100}


 -- estoy agregando  un  cambio

 -- agregando tercer cambi

 -- PARA REVISAR CAMBIO DESDE LA NOTEBBOK
 