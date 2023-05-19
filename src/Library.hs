module Library where
import PdePreludat
import GHC.Conc (setAllocationCounter)
import GHC.Num (Num)



data Bolita = Rojo | Azul | Verde | Negro  deriving (Eq, Show)

data Direccion = Norte | Sur | Este | Oeste deriving (Eq, Show)


-- replicar :: Number -> Number -> [Number]
-- replicar valor veces = valor :replicar valor (veces -1)

-- tablero :: Number -> Number -> [(Number,Number)]
-- tablero filas columnas = zip (map (+1) (replicar (-1) filas)) [0..(columnas -1)] ++ tablero filas columnas



-- moverDireccion :: Direccion -> [Number] ->[Number] --Hacer verificacion de que existe la c-1 /c+1 etc creo que mejor tupla el cabezal
-- moverDireccion Norte (f:c:fs) = (f-1:c:fs)
-- moverDireccion Sur (f:c:fs)=(f+1:c:fs)
-- moverDireccion Este (f:c:fs)=(f:c+1:fs)
-- moverDireccion Oeste (f:c:fs)=(f:c-1:fs)

-- type Espacio =((Bolita, Number),(Bolita, Number),(Bolita, Number),(Bolita, Number))

--(!!)((!!) tablero col) fila me devuelve una lista de tuplas

type Espacio = (Number,Number,Number,Number) -- cantidad de Rojo,Azul,Verde,Negro



-- sumar Rojo = (Rojo,cant+1)

generarFilas :: Number -> [Espacio]
generarFilas cant
    |   cant ==1 = [(0,0,0,0)]
    |   otherwise = ((0,0,0,0)) : generarFilas (cant-1)

inicializar :: Number -> Number -> [[Espacio]]
inicializar filas col
    | col==1 = [generarFilas filas]
    | otherwise = generarFilas filas : inicializar filas (col -1)

modificar :: [[Espacio]] -> Number->Number -> Bolita->[[Espacio]]
modificar tablero fila col color
    | 




elementoN::Number -> Espacio ->Number
elementoN 1 (x,_,_,_) =x
elementoN 2 (_,y,_,_) = y
elementoN 3 (_,_,z,_)=z
elementoN 4 (_,_,_,w)=w

ponerBola:: Number->Number ->[[Espacio]]-> Bolita ->[[Espacio]]
ponerBola fila col tablero bolita=  modificar tablero

sumar :: Bolita -> Espacio ->Espacio
sumar Rojo (x,y,z,w) = (x+1,y,z,w)
sumar Azul (x,y,z,w) = (x,y+1,z,w)
sumar Verde (x,y,z,w)=(x,y,z+1,w)
sumar Negro (x,y,z,w)= (x,y,z,w+1)


sacarBola = implementame





























