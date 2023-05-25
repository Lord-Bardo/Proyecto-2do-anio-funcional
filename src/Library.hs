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


generarFilas :: Number -> [Espacio] --replicate
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
moverDireccion :: Direccion ->Programa -> Programa --podemos abstraer el cabezal
moverDireccion Norte (Programa tablero cabezal) =Programa tablero (Cabezal (fila cabezal -1) (col cabezal))
moverDireccion Sur (Programa tablero cabezal) = Programa tablero (Cabezal (fila cabezal +1) (col cabezal))
moverDireccion Este (Programa tablero cabezal)  =Programa tablero (Cabezal (fila cabezal) (col cabezal +1))
moverDireccion Oeste (Programa tablero cabezal) =Programa tablero (Cabezal (fila cabezal) (col cabezal -1))

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
                  

alternativa ::(Programa -> Bool) ->[Programa->Programa] -> [Programa->Programa] -> Programa -> Programa
alternativa condicion conjunto1 conjunto2 p
    |   condicion p = accion conjunto1 p
    |   otherwise = accion conjunto2 p

-- hacer alternativa en funcion de si, sino o alreves
si :: (Programa->Bool) -> [Programa->Programa] ->Programa ->Programa
si condicion conjunto p
    |   condicion p = accion conjunto p
    |   otherwise = p


siNo ::(Programa->Bool) -> [Programa->Programa] ->Programa ->Programa
siNo condicion = si (not . condicion)

mientras :: (Programa->Bool) -> [Programa->Programa] ->Programa ->Programa --podemos usar sino
mientras condicion conjunto programa
    |   not (condicion programa) = programa
    |   otherwise =mientras condicion conjunto (accion conjunto programa)

conjuntoA :: [Programa -> Programa]
conjuntoA=[mover Sur,sumar Negro,sumar Negro, sumar Azul,mover Sur, repetir [sumar Rojo, sumar Azul] 15]

conjuntoB= [alternativa (hayBolita Verde) [mover Este, sumar Negro] [mover Norte, mover Este, sumar Azul]]

conjuntoC= [mientras ((<=9) . cantidadBolitas Verde) [sumar Verde]]


tablero2 = inicializar 5 5
cabezal2 = Cabezal 3 3
programa2 = Programa tablero2 cabezal2
punto7 = accion (conjuntoA ++ conjuntoB ++ [mover Este] ++ conjuntoC++ [sumar Azul]) programa2



