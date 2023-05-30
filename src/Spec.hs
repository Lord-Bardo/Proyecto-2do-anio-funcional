module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP Integrador" $ do
        it "test de prueba" $ do
            2 + 2 `shouldBe` 4
    describe "inicializar" $ do
        it  "crear tablero 2x2" $ do
            inicializar 2 2 `shouldBe`  [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]]
        it "crear tablero vacio (0x0)" $ do
            inicializar 0 0 `shouldBe` []
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
    describe "repetir" $ do
        it "repito 0 veces un conjunto de acciones sobre un Programa" $ do
            repetir conjuntoB 0 programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,0,0,0)]] Cabezal {fila = 2, col = 1}
        it "repito 2 veces un conjunto de acciones sobre un Programa" $ do
            repetir conjuntoB 2 programaTest `shouldBe` Programa [[(0,0,0,0),(0,0,0,0)],[(0,0,0,0),(0,2,0,0)]] Cabezal {fila = 4, col = 3}
    
        
            
