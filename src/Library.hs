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
moverDireccion Oeste cabezal= Cabezal (fila cabezal) ((col cabezal) -1) --VERIDICACION CAERSE TABLERO any


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


-- sumarBolita :: [Espacio] -> Number-> Number -> Bolita->[Espacio] --recibe las filas
-- sumarBolita [] _ _ _ = []
-- sumarBolita lista fila posicion color
--     |   posicion == length lista = [queDevuelvo lista fila posicion color]
--     |   otherwise = queDevuelvo lista fila posicion color : sumarBolita (tail lista) fila (posicion+1) color

-- queDevuelvo :: [Espacio] -> Number -> Number -> Bolita ->Espacio
-- queDevuelvo lista fila posicion color
--     | fila == posicion = sumar color (head lista)
--     | otherwise = head lista


-- ponerBola :: [[Espacio]] ->Cabezal->Number ->Bolita ->[[Espacio]] -- Primera posicion va  a ser 0
-- ponerBola [] _ _ _= []
-- ponerBola tablero cabezal posicion color
--     |   1 == length tablero = [temporal tablero (fila cabezal) (col cabezal) posicion color]
--     |   otherwise = temporal tablero (fila cabezal) (col cabezal) posicion color : ponerBola (tail tablero) cabezal (posicion +1) color

-- temporal :: [[Espacio]]-> Number->Number ->Number->Bolita ->[Espacio] -- Ultima posicion es col-1
-- temporal tablero fila col posicion color
--     |   col == posicion = sumarBolita (head tablero) fila 0 color
--     |   otherwise = head tablero

-- Dada una lista, una función y una posición, 
-- aplicar la función a ese elemento de la lista 
-- y devolver la lista habiendo transformado solo ese zip [0..] [a]

modificarElementoN:: [Espacio] ->Bolita-> (Bolita->Espacio->Espacio) ->Number->[Espacio]
modificarElementoN [] _ _ _= []
modificarElementoN lista bolita funcion fila =(init (primeraParte lista fila)) ++ (funcion bolita (last (primeraParte lista fila))  : (ultimaParte lista fila))

cambiarEspacio :: [[Espacio]] ->Bolita ->(Bolita->Espacio->Espacio)->Cabezal ->[[Espacio]]
cambiarEspacio [] _ _ _ =[]
cambiarEspacio tablero color funcion cabezal = (init (primeraParte tablero (col cabezal))) ++ (modificarElementoN (last (primeraParte tablero (col cabezal))) color funcion (fila cabezal)   : (ultimaParte tablero (col cabezal)))
                                                                                                                                                                            
primeraParte:: [a] ->Number -> [a] 
primeraParte lista n = take n lista

ultimaParte :: [a]-> Number ->[a]
ultimaParte lista n = drop n lista

--(zip [0..] lista)
--        3 [a,b,c,d,e,f,g]
-- take fila lista =[a,b,c] =primera parte
-- take inversa (length - fila) =[d,e,f,g] =ultimaparte
-- init (primera parte) ++funcion (last primeraParte) ++ ultima parte


-- ponerBola:: Number->Number ->[[Espacio]]-> Bolita ->[[Espacio]]
-- ponerBola fila col tablero bolita=  sumarBolita tablero

sumar :: Bolita -> Espacio ->Espacio
sumar Rojo (x,y,z,w) = (x+1,y,z,w)
sumar Azul (x,y,z,w) = (x,y+1,z,w)
sumar Verde (x,y,z,w)=(x,y,z+1,w)
sumar Negro (x,y,z,w)= (x,y,z,w+1)

restar :: Bolita -> Espacio ->Espacio --Verificar que no sea 0 la cantidad de bolitas
restar Rojo (x,y,z,w) = (x-1,y,z,w)
restar Azul (x,y,z,w) = (x,y-1,z,w)
restar Verde (x,y,z,w)=(x,y,z-1,w)
restar Negro (x,y,z,w)= (x,y,z,w-1)


-- Puede moverse el cabezal: que dada una dirección y un tablero nos dice si mover el cabezal del tablero en esa dirección no provoca que este se caiga del mismo.
-- Hay bolita de un color dado: nos retorna si hay alguna bolita de cierto color en la celda actual.
-- Cantidad de bolitas: nos retorna la cantidad de bolitas de un color dado que se encuentran en la celda actual del tablero.

elementoN::Bolita ->Espacio ->Number
elementoN Rojo (x,_,_,_) =x
elementoN Azul (_,y,_,_) = y
elementoN Verde (_,_,z,_)=z
elementoN Negro (_,_,_,w)=w


devolverEspacioPosicion :: [[Espacio]] -> Cabezal   -> Espacio
devolverEspacioPosicion tablero (Cabezal fila columna) = (last . take fila . last . take columna)tablero

hayBolita :: [[Espacio]] ->Cabezal -> Bolita -> Bool
hayBolita tablero cabezal color =cantidadBolitas tablero cabezal color >0

cantidadBolitas :: [[Espacio]] -> Cabezal -> Bolita -> Number
cantidadBolitas tablero cabezal color = elementoN color (devolverEspacioPosicion tablero cabezal)




















