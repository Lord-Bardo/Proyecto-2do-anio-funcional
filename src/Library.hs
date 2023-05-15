module Library where
import PdePreludat
import GHC.Conc (setAllocationCounter)
import GHC.Num (Num)



data Bolita = Rojo | Azul | Vede | Negro |Vacio
data Direccion = Norte | Sur | Este | Oeste


-- replicar :: Number -> Number -> [Number]
-- replicar valor veces = valor :replicar valor (veces -1)

-- tablero :: Number -> Number -> [(Number,Number)]
-- tablero filas columnas = zip (map (+1) (replicar (-1) filas)) [0..(columnas -1)] ++ tablero filas columnas


generarFilas :: Number -> [Bolita]
generarFilas cant
    |   cant ==1 = [Vacio]
    |   otherwise = Vacio : generarFilas (cant-1)

columnas :: Number -> Number -> [[Bolita]]
columnas filas col
    | col==1 = [generarFilas filas]
    | otherwise = generarFilas filas : columnas filas (col -1)



ponerBola = implementame

sacarBola = implementame





























