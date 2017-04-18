module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón), NaveEspacial(Módulo, Base), Dirección(Babor, Estribor), TipoPeligro(Pequeño, Grande, Torpedo), Peligro, foldNave, capacidad, poderDeAtaque, puedeVolar, mismoPotencial, mayorCapacidad, transformar, impactar, maniobrar, pruebaDeFuego, componentesPorNivel, dimensiones) where

data Componente = Contenedor | Motor | Escudo | Cañón deriving (Eq, Show)

data NaveEspacial = Módulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

data Dirección = Babor | Estribor deriving Eq

data TipoPeligro = Pequeño | Grande | Torpedo deriving Eq

type Peligro = (Dirección, Int, TipoPeligro)

instance Show NaveEspacial where
  show = ("\n" ++) . (padNave 0 0 False)
  
padNave nivel acum doPad (Base c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padNave nivel acum doPad (Módulo x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++ 
					  pad 4 ++ padNave (nivel+1) (acum+l) False i ++ "\n" ++
					  padNave (nivel+1) (acum+l) True d where l = length $ show x

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1
foldNave :: (Componente -> b) -> (Componente -> b -> b -> b) -> NaveEspacial -> b
foldNave fBase fMod (Base componente) = fBase componente
foldNave fBase fMod (Módulo componente nave1 nave2) = fMod componente (recu nave1) (recu nave2)
														where recu = foldNave fBase fMod

sonComponentesIguales :: Componente -> Componente -> Bool
sonComponentesIguales c1 c2 = (c1 == c2)

cantidadComponentesNave :: NaveEspacial -> Componente -> Int
cantidadComponentesNave = foldNave (\c -> (\componente -> if sonComponentesIguales c componente then 1 else 0))
	(\c nave1 nave2 -> (\componente -> nave1 componente + nave2 componente + if sonComponentesIguales c componente then 1 else 0))

--Ejercicio 2
capacidad :: NaveEspacial -> Int
capacidad nave = cantidadComponentesNave nave Contenedor

poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque nave = cantidadComponentesNave nave Cañón

puedeVolar :: NaveEspacial -> Bool
puedeVolar nave = (cantidadComponentesNave nave Motor ) > 0

mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial nave1 nave2 = and [(cantidadComponentesNave nave1 c) == (cantidadComponentesNave nave2 c) | c <- [Contenedor, Motor, Escudo, Cañón] ]

--Ejercicio 3

mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = foldr1 (\x recu -> if capacidad x > capacidad recu then x else recu)

--Ejercicio 4

transformarEnContenedor :: Componente -> Componente
transformarEnContenedor c = Contenedor

transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar f = foldNave (\c -> Base (f c)) (\c nave1 nave2 -> Módulo (f c) nave1 nave2)

-- Ejercicio 5

{- no se puede usar foldNave en este caso porque el peligro no se aplica recursivamente a las dos
subnaves, que es la idea de foldNave
en cambio, lo que pasa es que si el peligro viene de Babor se aplica sobre la subnave izquierda
y si viene de Estribor, sobre la subnave derecha. -}

impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar (dir, lvl, tipo) (Base componente) = 
	if lvl == 0 then
		(if tipo == Pequeño && componente == Escudo 
			then Base Escudo
			else Base Contenedor)
	else (Base componente)
impactar (dir, lvl, tipo) (Módulo componente nave1 nave2) =
	if lvl == 0 then
		(if (tipo == Pequeño && componente == Escudo) || (tipo == Grande && componente == Escudo && (protegido nave1 || protegido nave2))
			then Módulo componente nave1 nave2
			else Base Contenedor)
	else (if dir == Babor
		then Módulo componente (impactar (dir, lvl-1, tipo) nave1) nave2
		else Módulo componente nave1 (impactar (dir, lvl-1, tipo) nave2))

protegido :: NaveEspacial -> Bool
protegido n = (poderDeAtaque n) > 0

-- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar nave peligros = foldl (\recu x -> impactar x recu) nave peligros

-- Ejercicio 7

{- para este ejercicio devolvemos las naves que sobreviven en el estado
posterior a haber pasado por los peligros -}
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego peligros naves = filter (\n -> puedeVolar n) (despuesDeManiobrar peligros naves)

despuesDeManiobrar :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
despuesDeManiobrar peligros = map (\n -> maniobrar n peligros)

-- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel = foldNave (\_ -> (\i -> if i == 0 then 1 else 0))
	(\_ recu1 recu2 -> (\i -> if i == 0 then 1 else (recu1 (i-1)) + (recu2 (i-1))))

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones nave = (largo nave, ancho nave) 

largo :: NaveEspacial -> Int
largo = foldNave (\_ -> 1) (\_ recu1 recu2 -> (max recu1 recu2) + 1)

ancho :: NaveEspacial -> Int
ancho nave = maximoHastaCero [componentesPorNivel nave nivel | nivel <- [0..]]

{- devuelve el máximo de la lista considerada hasta el primer cero que aparece adentro
el objetivo es poder pasarle la lista de componentesPorNivel que es infinita pero en algun momento 
empieza a valer cero -}
maximoHastaCero :: [Int] -> Int
maximoHastaCero = foldr (\x recu -> if x == 0 then 0 else max x recu) 0
