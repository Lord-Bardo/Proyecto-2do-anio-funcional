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

type Espacio = (Number,Number,Number,Number) -- cantidad de Rojo,Azul,Verde,Negro

type Sentencia = Programa ->Programa

data Programa = Programa {
    tablero:: [[Espacio]],
    cabezal:: Cabezal
}deriving (Eq, Show)


generarFilas :: Number -> [Espacio]
generarFilas cant
    |   cant ==1 = [(0,0,0,0)]
    |   otherwise = (0,0,0,0) : generarFilas (cant-1)

inicializar :: Number -> Number -> [[Espacio]]
inicializar filas col
    | col==1 = [generarFilas filas]
    | otherwise = generarFilas filas : inicializar filas (col -1)



mover :: Direccion -> Programa ->Programa
mover = moverDireccion 

sumar::Bolita->Programa->Programa
sumar = cambiarEspacio suma

restar::Bolita->Programa->Programa
restar = cambiarEspacio resta



-- sentencia restar a= modificarElementoN
-- sentencia mover a= moverDireccion a cabezal
moverDireccion :: Direccion ->Programa -> Programa --Hacer verificacion de que existe la c-1 /c+1 etc creo que mejor tupla el cabezal
moverDireccion Norte (Programa tablero cabezal) =Programa tablero (Cabezal ((fila cabezal) -1) (col cabezal)) 
moverDireccion Sur (Programa tablero cabezal) = Programa tablero (Cabezal ((fila cabezal) +1) (col cabezal)) 
moverDireccion Este (Programa tablero cabezal)  =Programa tablero (Cabezal (fila cabezal) ((col cabezal) +1)) 
moverDireccion Oeste (Programa tablero cabezal) =Programa tablero (Cabezal (fila cabezal) ((col cabezal) -1)) 

modificarElementoN:: [Espacio] ->Bolita-> (Bolita->Espacio->Espacio) ->Number->[Espacio]
modificarElementoN [] _ _ _= []
modificarElementoN lista bolita funcion fila =(init (primeraParte lista fila)) ++ (funcion bolita (last (primeraParte lista fila))  : (ultimaParte lista fila))

cambiarEspacio :: (Bolita->Espacio->Espacio)-> Bolita-> Programa ->Programa
cambiarEspacio funcion  color (Programa tablero cabezal) = Programa ((init (primeraParte tablero (col cabezal))) ++ (modificarElementoN (last (primeraParte tablero (col cabezal))) color funcion (fila cabezal)   : (ultimaParte tablero (col cabezal)))) cabezal
                                                                                                                                                                            
primeraParte:: [a] ->Number -> [a] 
primeraParte lista n = take n lista

ultimaParte :: [a]-> Number ->[a]
ultimaParte lista n = drop n lista


-- existe :: [[Espacio]] -> Direccion -> Cabezal -> Bool
-- existe tablero direccion cabezal = col (moverDireccion direccion cabezal) >= 0 &&  col (moverDireccion direccion cabezal)<= length tablero && fila (moverDireccion direccion cabezal) >= 0 &&  fila (moverDireccion direccion cabezal)<= (length . head) tablero




suma :: Bolita -> Espacio ->Espacio
suma Rojo (x,y,z,w) = (x+1,y,z,w)
suma Azul (x,y,z,w) = (x,y+1,z,w)
suma Verde (x,y,z,w)=(x,y,z+1,w)
suma Negro (x,y,z,w)= (x,y,z,w+1)

resta :: Bolita -> Espacio ->Espacio --Verificar que no sea 0 la cantidad de bolitas
resta Rojo (x,y,z,w) = (x-1,y,z,w)
resta Azul (x,y,z,w) = (x,y-1,z,w)
resta Verde (x,y,z,w)=(x,y,z-1,w)
resta Negro (x,y,z,w)= (x,y,z,w-1)


elementoN::Bolita ->Espacio ->Number
elementoN Rojo (x,_,_,_) =x
elementoN Azul (_,y,_,_) = y
elementoN Verde (_,_,z,_)=z
elementoN Negro (_,_,_,w)=w

devolverEspacioPosicion :: Programa -> Espacio
devolverEspacioPosicion p = last (take (filaCabezal p) (last (take (colCabezal p) (tablero p))))

filaCabezal :: Programa -> Number
filaCabezal p = (fila . cabezal) p

colCabezal :: Programa ->Number
colCabezal p = (col . cabezal) p

hayBolita :: Programa -> Bolita -> Bool
hayBolita p color =cantidadBolitas color p > 0

cantidadBolitas :: Bolita ->  Programa -> Number
cantidadBolitas color p = elementoN color (devolverEspacioPosicion p)
 

-- Repetir una determinada cantidad de veces un conjunto de sentencias sobre un tablero dado

c = Cabezal 1 1
t= inicializar 3 4 
p= Programa t c

conjunto1 = [mover Sur, sumar Verde,mover Este, restar Verde]
conjunto2= [mover Norte, sumar Azul , mover Oeste, sumar Rojo]

repetir:: [Programa->Programa] ->Number -> Programa -> Programa
repetir lista 0 p = p
repetir lista 1 p = accion lista p
repetir lista r p = accion lista (repetir lista (r-1) p)  
 
accion :: [Programa->Programa]-> Programa -> Programa
accion lista p = foldl(\p funcion -> funcion p) p lista  

--La sentencia alternativa: recibe una condición que se aplica sobre un tablero y dos conjuntos de sentencias
--  y un tablero. Si la condición aplicada al tablero es verdadera 
--  ejecuta sobre el tablero el primer conjunto de sentencias.
--  Si es falsa ejecuta el segundo grupo de sentencias.

alternativa ::(Programa -> Bool)=>(Programa -> Bool) ->[Programa->Programa] -> [Programa->Programa] -> Programa -> Programa
alternativa condicion conjunto1 conjunto2 p
    |   condicion p = repetir conjunto1 1 p
    |   otherwise = repetir conjunto2 1 p


