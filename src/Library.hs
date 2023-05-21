{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Library where
import PdePreludat
import GHC.Conc (setAllocationCounter)
import GHC.Num (Num)


data Cabezal = Cabezal {
    fila:: Number,
    col :: Number
}deriving (Eq, Show)


data Bolita = Rojo | Azul | Verde | Negro  deriving (Eq, Show)

data Direccion = Norte | Sur | Este | Oeste deriving (Eq, Show)


-- replicar :: Number -> Number -> [Number]
-- replicar valor veces = valor :replicar valor (veces -1)

-- tablero :: Number -> Number -> [(Number,Number)]
-- tablero filas columnas = zip (map (+1) (replicar (-1) filas)) [0..(columnas -1)] ++ tablero filas columnas



moverDireccion :: Direccion -> Cabezal ->Cabezal --Hacer verificacion de que existe la c-1 /c+1 etc creo que mejor tupla el cabezal
moverDireccion Norte  cabezal= Cabezal ((fila cabezal) -1) (col cabezal)
moverDireccion Sur cabezal= Cabezal ((fila cabezal) +1) (col cabezal)
moverDireccion Este cabezal= Cabezal (fila cabezal) ((col cabezal) +1)
moverDireccion Oeste cabezal= Cabezal (fila cabezal) ((col cabezal) -1)

-- type Espacio =((Bolita, Number),(Bolita, Number),(Bolita, Number),(Bolita, Number))

--(!!)((!!) tablero col) fila me devuelve una lista de tuplas

type Espacio = (Number,Number,Number,Number) -- cantidad de Rojo,Azul,Verde,Negro



-- sumar Rojo = (Rojo,cant+1)

generarFilas :: Number -> [Espacio]
generarFilas cant
    |   cant ==1 = [(0,0,0,0)]
    |   otherwise = (0,0,0,0) : generarFilas (cant-1)

inicializar :: Number -> Number -> [[Espacio]]
inicializar filas col
    | col==1 = [generarFilas filas]
    | otherwise = generarFilas filas : inicializar filas (col -1)
cabezal = Cabezal 0 0


-- sumarBolita :: [[Espacio]] -> Number->Number -> Bolita->[[Espacio]]
-- sumarBolita tablero fila col color
--     | 


sumarBolita :: [Espacio] -> Number-> Number -> Bolita->[Espacio]
sumarBolita [] _ _ _ = []
sumarBolita lista fila posicion color
    |   posicion == length lista = [queDevuelvo lista fila posicion color]
    |   otherwise = queDevuelvo lista fila posicion color : sumarBolita (tail lista) fila (posicion+1) color

ponerBola :: [[Espacio]] ->Cabezal->Number ->Bolita ->[[Espacio]] -- Primera posicion va  a ser 0
ponerBola [] _ _ _= []
ponerBola tablero cabezal posicion color
    |   1 == length tablero = [temporal tablero (fila cabezal) (col cabezal) posicion color]
    |   otherwise = temporal tablero (fila cabezal) (col cabezal) posicion color : ponerBola (tail tablero) cabezal (posicion +1) color

temporal :: [[Espacio]]-> Number->Number ->Number->Bolita ->[Espacio] -- Ultima posicion es col-1
temporal tablero fila col posicion color
    |   col == posicion = sumarBolita (head tablero) fila 0 color
    |   otherwise = head tablero

queDevuelvo :: [Espacio] -> Number -> Number -> Bolita ->Espacio
queDevuelvo lista fila posicion color
    | fila == posicion = sumar color (head lista)
    | otherwise = head lista


elementoN::Number -> Espacio ->Number
elementoN 1 (x,_,_,_) =x
elementoN 2 (_,y,_,_) = y
elementoN 3 (_,_,z,_)=z
elementoN 4 (_,_,_,w)=w

-- ponerBola:: Number->Number ->[[Espacio]]-> Bolita ->[[Espacio]]
-- ponerBola fila col tablero bolita=  sumarBolita tablero

sumar :: Bolita -> Espacio ->Espacio
sumar Rojo (x,y,z,w) = (x+1,y,z,w)
sumar Azul (x,y,z,w) = (x,y+1,z,w)
sumar Verde (x,y,z,w)=(x,y,z+1,w)
sumar Negro (x,y,z,w)= (x,y,z,w+1)


sacarBola = implementame





























