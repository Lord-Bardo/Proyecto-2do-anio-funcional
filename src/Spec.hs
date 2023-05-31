module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "Testing de los puntos del tp integrador" $ do
        Punto1
        Punto2
        Punto3
        Punto4
        Punto5
        Punto6
        Punto7

Punto1 = $do --Mostrar celdas, bolitas, un tablero, y las direcciones
    describe "implementame" $do
        it "implementame" $do
Punto2 = $do
    describe "TP Modelar las estructuras básicas del lenguaje:" $ do --Hay que agregar el cabezal aca
        describe "a) El tablero" $ do
            let cabezal1 = Cabezal 3 1
            it  "crear tablero 2x2" $ do
                inicializar 2 2 `shouldBe`  [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] cabezal1
            it "crear tablero vacio (0x0)" $ do
                inicializar 0 0 `shouldBe` Programa [] cabezal1
            it "tablero e 3x5 " $do
                punto2 3 5 `shouldBe` Programa [[(0,0,0,0),(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0),(0,0,0,0)]] cabezal1                  
Punto3 = $do
    describe "mover" $ do
        it "mover al Norte" $ do
            mover Norte programaTest  `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 1, col = 1}
        it "mover al Este" $ do
            mover Este programaTest `shouldBe`  Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 2}
    describe "suma" $ do
        it "pruebo sumar Rojo a un tablero vacio" $ do
            sumar Rojo programaTest `shouldBe` Programa [[(0,0,0,0),(1,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo sumar Azul a un tablero vacio" $  do
            sumar Azul programaTest `shouldBe` Programa [[(0,0,0,0),(0,1,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo sumar Verde a un tablero vacio" $ do
            sumar Verde programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,1,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo sumar Negro a un tablero vacio" $ do
            sumar Negro programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,1)],[(0,0,0,0),(0,0,0,0)]]  Cabezal {fila = 2, col = 1}
    describe "restar" $ do
        it "pruebo restar Rojo a una posicion sin bolitas" $ do
            restar Rojo programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo restar Azul a una posicion sin bolitas" $ do
            restar Azul programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo restar Verde a una posicion sin bolitas" $ do
            restar Verde programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo restar Negro a una posicion sin bolitas" $ do
            restar Negro programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo restar Rojo a una posicion con bolitas" $ do
            restar Rojo programaTest2 `shouldBe` Programa [[(0,0,0,0),(0,1,1,1)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo restar Azul a una posicion con bolitas" $ do
            restar Azul programaTest2 `shouldBe` Programa [[(0,0,0,0),(1,0,1,1)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo restar Verde a una posicion con bolitas" $ do
            restar Verde programaTest2 `shouldBe` Programa [[(0,0,0,0),(1,1,0,1)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "pruebo restar Negro a una posicion con bolitas" $ do
            restar Negro programaTest2 `shouldBe` Programa [[(0,0,0,0),(1,1,1,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}

Punto4 = $do
    describe "Codificar sentencias compuestas, tales como:" $ do
            describe "a) Repetir una determinada cantidad de veces un conjunto de sentencias sobre un tablero dado" $ do -- podes declarar estos tableros asi no los copias todo el tiempo
                it "repito 0 veces un conjunto de acciones sobre un Programa" $ do
                    repetir conjuntoB 0 programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
                it "repito 2 veces un conjunto de acciones sobre un Programa" $ do
                    repetir conjuntoB 2 programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,2,0,0)]] Cabezal {fila = 4, col = 3}
    
Punto5 = $do
    describe "Codificar las siguientes expresiones para saber si:" $do
        it "a) Puede moverse el cabezal: que dada una dirección y un tablero nos dice si mover el cabezal del tablero en esa dirección no provoca que este se caiga del mismo." $do

        it "b) Hay bolita de un color dado: nos retorna si hay alguna bolita de cierto color en la celda actual." $do

        it "c) Cantidad de bolitas: nos retorna la cantidad de bolitas de un color dado que se encuentran en la celda actual del tablero" $do


Punto6 = $do
    describe "Codificar la instrucción programa, que recibe un tablero y una lista de sentencias y retorna el tablero resultante de aplicar todas las sentencias sucesivamente sobre este." $do
        it "implementame" $ do
            
Punto7 = $do
    describe "Escribir este programa de gobstones en haskell, partiendo de un tablero vacío de tres por tres" $do
        it "mover Norte,sumar Negro,sumar Negro, sumar Azul,mover Norte, repetir [sumar Rojo, sumar Azul] 15 ,alternativa (hayBolita Verde) [mover Este, sumar Negro] [mover Sur, mover Este, sumar Azul],mientras ((<=9) . cantidadBolitas Verde) [sumar Verde] " $do
            punto7 `shouldBe` Programa [[(15,15,0,0),(0,1,0,2),(0,0,0,0)],[(0,0,0,0),(0,1,0,0),(0,0,0,0)],[(0,0,0,0),(0,1,10,0),(0,0,0,0)]] (Cabezal 2 3)
            
