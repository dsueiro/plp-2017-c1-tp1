module Main where
import NavesEspaciales
import Test.HUnit
import Data.List



--Naves para pruebas:
contenedorSolo = Base Contenedor
nave1 = Base Motor
nave2 = Módulo Cañón (Base Escudo) (Base Motor)
nave3 = Módulo Motor (Base Escudo) (Base Cañón)
nave4 = Módulo Contenedor nave2 nave3
nave5 = Módulo Contenedor nave3 nave2
nave6 = Módulo Contenedor nave4 nave1
nave7 = Módulo Contenedor nave1 nave5
nave8 = Módulo Contenedor nave1 nave6
nave9 = Módulo Escudo 
		(Módulo Escudo (Módulo Escudo (Base Escudo) (Base Cañón)) (Módulo Motor (Base Contenedor) (Base Motor))) 
		(Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))

soloUnMotor = Base Motor
puroContenedor = Módulo Contenedor (Base Contenedor) (Base Contenedor)
tresCañones = Módulo Cañón (Base Cañón) (Base Cañón)

contenedorYCañon = Módulo Contenedor (Base Cañón) (Base Contenedor)
otroCañon = Módulo Contenedor (Base Contenedor) (Base Cañón)

escudoSinCañon = Módulo Escudo (Base Contenedor) (Base Contenedor)

protegido = Módulo Escudo (Base Contenedor) (Base Cañón)
protegidoNivel1Estribor = Módulo Contenedor soloUnMotor protegido

superProtegido = Módulo Motor protegido protegido

desbalanceado = Módulo Escudo (Base Contenedor) protegido


--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8
  ]

testsEj2 = test [
  0 ~=? capacidad soloUnMotor,
  3 ~=? capacidad puroContenedor,
  3 ~=? capacidad nave9,
  1 ~=? capacidad contenedorSolo,
  2 ~=? capacidad superProtegido
  ]

testsEj3 = test [
  contenedorSolo ~=? mayorCapacidad [contenedorSolo, soloUnMotor],
  nave8 ~=? mayorCapacidad [nave6, nave8, contenedorSolo, soloUnMotor],
  puroContenedor ~=? mayorCapacidad [puroContenedor, contenedorSolo, otroCañon]
  ]

testsEj4 = test [
  soloUnMotor ~=? transformar (const Motor) soloUnMotor,
  contenedorSolo ~=? transformar (const Contenedor) soloUnMotor,
  nave3 ~=? transformar (\c -> if c == Motor then Cañón else (if c == Cañón then Motor else c)) nave2,
  Módulo Contenedor 
    (Módulo Contenedor (Módulo Contenedor (Base Contenedor) (Base Cañón)) (Módulo Motor (Base Contenedor) (Base Motor))) 
    (Módulo Contenedor (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Contenedor (Base Cañón) (Base Contenedor)))
    ~=? transformar (\c -> if c == Escudo then Contenedor else c) nave9,
  Módulo Contenedor 
    (Módulo Contenedor (Módulo Contenedor (Base Contenedor) (Base Contenedor)) (Módulo Contenedor (Base Contenedor) (Base Contenedor))) 
    (Módulo Contenedor (Módulo Contenedor (Base Contenedor) (Base Contenedor)) (Módulo Contenedor (Base Contenedor) (Base Contenedor)))
    ~=? transformar (const Contenedor) nave9
  ]

testsEj5 = test [
  nave2 ~=? impactar (Babor, 1, Pequeño) nave2,
  Módulo Cañón (Base Escudo) (Base Contenedor) ~=? impactar (Estribor, 1, Pequeño) nave2,
  Base Contenedor ~=? impactar (Babor, 0, Pequeño) nave1,
  nave1 ~=? impactar (Babor, 1, Pequeño) nave1,
  Módulo Contenedor (Base Contenedor) (Módulo Motor (Base Escudo) (Base Cañón)) ~=? impactar (Babor, 1, Pequeño) nave4,
  protegido ~=? impactar (Estribor, 0, Grande) protegido,
  protegido ~=? impactar (Babor, 0, Pequeño) protegido,
  contenedorSolo ~=? impactar (Babor, 0, Torpedo) protegido,
  protegidoNivel1Estribor ~=? impactar (Estribor, 1, Grande) protegidoNivel1Estribor,
  Módulo Contenedor (Base Motor) (Base Contenedor) ~=? impactar (Estribor, 1, Torpedo) protegidoNivel1Estribor,
  contenedorSolo ~=? impactar (Babor, 0, Grande) escudoSinCañon,
  escudoSinCañon ~=? impactar (Babor, 0, Pequeño) escudoSinCañon
  ]

testsEj6 = test [
  Módulo Contenedor (Base Contenedor) (Módulo Motor (Base Escudo) (Base Contenedor)) ~=? maniobrar nave4 [(Babor, 2, Pequeño), (Babor, 1, Pequeño), (Estribor, 2, Grande)],
  superProtegido ~=? maniobrar superProtegido [(Babor, 1, Grande), (Estribor, 1, Grande), (Babor, 1, Pequeño), (Estribor, 1, Pequeño)],
  Módulo Motor contenedorSolo protegido ~=? maniobrar superProtegido [(Babor, 1, Grande), (Babor, 1, Torpedo)],
  Módulo Motor contenedorSolo contenedorSolo ~=? maniobrar superProtegido [(Babor, 1, Torpedo), (Estribor, 1, Torpedo)]
  ]

testsEj7 = test [
  [nave9, Módulo Escudo contenedorSolo (Módulo Escudo (Base Motor) (Base Cañón))] ~=? pruebaDeFuego [(Babor, 1, Grande), (Estribor, 1, Grande)] [nave9, protegidoNivel1Estribor, desbalanceado, Módulo Escudo contenedorSolo (Módulo Escudo (Base Motor) (Base Cañón))],
  3 ~=? length (pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9])
  ]

testsEj8 = test [
  1 ~=? componentesPorNivel nave9 0,
  2 ~=? componentesPorNivel nave9 1, 
  4 ~=? componentesPorNivel nave9 2,
  8 ~=? componentesPorNivel nave9 3,
  0 ~=? componentesPorNivel nave9 4,
  2 ~=? componentesPorNivel desbalanceado 1,
  2 ~=? componentesPorNivel desbalanceado 2,
  (4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)]),
  (1,1) ~=? dimensiones contenedorSolo,
  (2,2) ~=? dimensiones nave2,
  (3,4) ~=? dimensiones nave4,
  (4,4) ~=? dimensiones nave6,
  (4,8) ~=? dimensiones nave9,
  (3,2) ~=? dimensiones desbalanceado
  ]


--Ejemplos de referencia para maniobrar:	
--maniobrar nave9 [(Babor, 0, Grande),(Babor,2,Torpedo),(Estribor,0,Pequeño)] destruye solo el subárbol izquierdo del subárbol izquierdo.
--maniobrar nave9 [(Estribor,0,Pequeño),(Babor,2,Torpedo),(Babor, 1, Grande)] destruye todo el subárbol izquierdo.