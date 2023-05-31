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

generarFilas :: Number ->[Espacio]
generarFilas veces = replicate veces (0,0,0,0)

inicializar :: Number -> Number -> [[Espacio]]
inicializar filas col = replicate col (generarFilas filas)

punto2 :: Number -> Number ->Programa --Nuestro 3 1 es el 1 1
punto2 fila columna = Programa (inicializar fila columna) (Cabezal 3 1)


mover :: Direccion -> Programa ->Programa
mover = moverDireccion

sumar::Bolita->Programa->Programa
sumar = cambiarEspacio suma

restar::Bolita->Programa->Programa
restar = cambiarEspacio restara

moverDireccion :: Direccion ->Programa -> Programa 
moverDireccion direccion (Programa tablero cabezal) =Programa tablero (dndeQuedo direccion cabezal)

dndeQuedo :: Direccion -> Cabezal ->Cabezal 
dndeQuedo Norte (Cabezal fila col) =Cabezal (fila-1) col
dndeQuedo Sur (Cabezal fila col) =Cabezal (fila +1) col 
dndeQuedo Este (Cabezal fila col)  =Cabezal fila (col+1)
dndeQuedo Oeste (Cabezal fila col) = Cabezal fila (col-1)

modificarElementoN:: [Espacio] ->Bolita-> (Bolita->Espacio->Espacio) ->Number->[Espacio]
modificarElementoN [] _ _ _= []
modificarElementoN lista bolita funcion fila =init (primeraParte lista fila) ++ (funcion bolita (last (primeraParte lista fila))  : ultimaParte lista fila)

cambiarEspacio :: (Bolita->Espacio->Espacio)-> Bolita-> Programa ->Programa
cambiarEspacio funcion  color (Programa tablero cabezal) = Programa (init (primeraParte tablero (col cabezal)) ++ (modificarElementoN (last (primeraParte tablero (col cabezal))) color funcion (fila cabezal)   : ultimaParte tablero (col cabezal))) cabezal

primeraParte:: [a] ->Number -> [a]
primeraParte lista n = take n lista

ultimaParte :: [a]-> Number ->[a]
ultimaParte lista n = drop n lista

suma :: Bolita -> Espacio ->Espacio --check
suma Rojo (x,y,z,w) = (x+1,y,z,w)
suma Azul (x,y,z,w) = (x,y+1,z,w)
suma Verde (x,y,z,w)=(x,y,z+1,w)
suma Negro (x,y,z,w)= (x,y,z,w+1)

resta :: Bolita -> Espacio ->Espacio --Verificar que no sea 0 la cantidad de bolitas pseudocheck
resta Rojo (x,y,z,w) = (x-1,y,z,w)
resta Azul (x,y,z,w) = (x,y-1,z,w)
resta Verde (x,y,z,w)=(x,y,z-1,w)
resta Negro (x,y,z,w)= (x,y,z,w-1)
restara :: Bolita -> Espacio -> Espacio
restara color (x,y,z,w)
    | elementoN color (x,y,z,w) > 0 = resta color (x,y,z,w)
    | otherwise = (x,y,z,w)

elementoN::Bolita ->Espacio ->Number
elementoN Rojo (x,_,_,_) =x
elementoN Azul (_,y,_,_) = y
elementoN Verde (_,_,z,_)=z
elementoN Negro (_,_,_,w)=w

devolverEspacioPosicion :: Programa -> Espacio
devolverEspacioPosicion p = last (take (filaCabezal p) (last (take (colCabezal p) (tablero p))))

filaCabezal :: Programa -> Number
filaCabezal = fila . cabezal

colCabezal :: Programa ->Number
colCabezal = col . cabezal

hayBolita :: Bolita ->Programa ->  Bool
hayBolita color p =cantidadBolitas color p > 0

cantidadBolitas :: Bolita ->  Programa -> Number
cantidadBolitas color p = elementoN color (devolverEspacioPosicion p)

-- Repetir una determinada cantidad de veces un conjunto de sentencias sobre un tablero dado

conjunto1 = [mover Sur, sumar Verde,mover Este, restar Verde]
conjunto2= [mover Norte, sumar Azul , mover Oeste, sumar Rojo]

repetir:: [Programa->Programa] ->Number -> Programa -> Programa --replicate podemos usar
repetir lista 0 p = p
repetir lista r p = accion lista (repetir lista (r-1) p)

accion :: [Programa->Programa]-> Programa -> Programa --algo aca creo
accion lista p = foldl(\p funcion -> funcion p) p lista

irAlBorde :: (Direccion -> Programa -> Programa) -> Direccion -> Programa -> Programa
irAlBorde funcion direccion programa = (last . take (cuantoMeMuevo direccion programa) . iterate (funcion direccion)) programa

cuantoMeMuevo:: Direccion -> Programa -> Number
cuantoMeMuevo  Norte programa = (fila . cabezal) programa
cuantoMeMuevo  Sur programa = (length . head . tablero) programa- (fila.cabezal)programa + 1
cuantoMeMuevo  Oeste programa = (col.cabezal)programa 
cuantoMeMuevo  Este programa =   (length .tablero) programa - (col.cabezal)programa + 1
                  
alternativa :: (Programa -> Bool) ->[Programa->Programa] -> [Programa->Programa] -> Programa -> Programa
alternativa condicion conjunto1 conjunto2 p = siNo condicion conjunto2 (si condicion conjunto1 p)

si :: (Programa->Bool) -> [Programa->Programa] ->Programa ->Programa
si condicion conjunto p
    |   condicion p = accion conjunto p
    |   otherwise = p

siNo ::(Programa->Bool) -> [Programa->Programa] ->Programa ->Programa
siNo condicion = si (not . condicion)

mientras :: (Programa->Bool) -> [Programa->Programa] ->Programa ->Programa 
mientras condicion conjunto programa
    |   not (condicion programa) = programa
    |   otherwise =mientras condicion conjunto (accion conjunto programa)

conjuntoA :: [Programa -> Programa]
conjuntoA=[mover Norte,sumar Negro,sumar Negro, sumar Azul,mover Norte, repetir [sumar Rojo, sumar Azul] 15]

conjuntoB= [alternativa (hayBolita Verde) [mover Este, sumar Negro] [mover Sur, mover Este, sumar Azul]]

conjuntoC= [mientras ((<=9) . cantidadBolitas Verde) [sumar Verde]]

tablero2 = inicializar 3 3
cabezal2 = Cabezal 3 1
programa2 = Programa tablero2 cabezal2
punto7 = accion (conjuntoA ++ conjuntoB ++ [mover Este] ++ conjuntoC++ [sumar Azul] ) programa2

tableroTest = inicializar 2 2
cabezalTest = Cabezal 2 1
programaTest = Programa tableroTest cabezalTest  
tableroTest2 =  [[(0,0,0,0),(1,1,1,1)],[(0,0,0,0),(0,0,0,0)]]
programaTest2 = Programa tableroTest2 cabezalTest