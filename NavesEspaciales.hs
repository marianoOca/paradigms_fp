module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón), NaveEspacial(Módulo, Base), Dirección(Babor, Estribor), TipoPeligro(Pequeño, Grande, Torpedo), Peligro, foldNave, capacidad, poderDeAtaque, puedeVolar, mismoPotencial, mayorCapacidad, transformar, impactar, maniobrar, pruebaDeFuego, componentesPorNivel, dimensiones) where

--BORRAR DESPUES

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

-- fin

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
foldNave fBase _ (Base x) = fBase x
foldNave fBase fModulo (Módulo x i d) = fModulo x (foldNave fBase fModulo i) (foldNave fBase fModulo d)


--Ejercicio 2.1
contarCantidad :: NaveEspacial -> Componente -> Int
contarCantidad laNave elComponente = foldNave (\compBase -> if compBase == elComponente then 1 else 0) (\compNave acumIzq acumDer-> acumIzq + acumDer +  if compNave == elComponente then 1 else 0) laNave

capacidad :: NaveEspacial -> Int
capacidad laNave = contarCantidad laNave Contenedor


--Ejercicio 2.2
poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque laNave = contarCantidad laNave Cañón


--Ejercicio 2.3
puedeVolar :: NaveEspacial -> Bool
puedeVolar laNave = (contarCantidad laNave Motor) > 0


--Ejercicio 2.4
mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial naveUno naveDos =  (contarCantidad naveUno Motor == contarCantidad naveDos Motor ) && (contarCantidad naveUno Escudo == contarCantidad naveDos Escudo) && (contarCantidad naveUno Cañón == contarCantidad naveDos Cañón ) && (contarCantidad naveUno Cañón == contarCantidad naveDos Cañón )


--Ejercicio 3
mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad listaNaves = foldr1 (\naveUno naveDos -> if capacidad naveUno > capacidad naveDos then naveUno else naveDos) listaNaves  


--Ejercicio 4
transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar f = foldNave (\comp -> Base (f comp)) (\comp naveIz naveDer -> Módulo (f comp) naveIz naveDer)


-- Ejercicio 5
-- fold nave no es conveniente porque recorre toda la estructura y para este ejercicio sólo queremos recorrer los costados de la nave
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar (_ ,         0 ,     elTipoPeligro) subNave               = golpearSubnave elTipoPeligro subNave
impactar (_ ,         nivel , _            ) (Base comp)           = Base comp
impactar (dirección , nivel , elTipoPeligro) (Módulo comp izq der) = if dirección == Babor then Módulo comp (impactar (Babor , nivel-1 , elTipoPeligro) izq) der else Módulo comp izq (impactar (Estribor , nivel-1 , elTipoPeligro) der)

golpearSubnave :: TipoPeligro -> NaveEspacial -> NaveEspacial
golpearSubnave Torpedo _                     = Base Contenedor
golpearSubnave Pequeño (Base comp)           = if comp == Escudo then Base comp else Base Contenedor
golpearSubnave Pequeño (Módulo comp izq der) = if comp == Escudo then Módulo comp izq der else Base Contenedor
golpearSubnave Grande  (Base _)              = Base Contenedor
golpearSubnave Grande  (Módulo comp izq der) = if poderDeAtaque izq + poderDeAtaque der > 0 then golpearSubnave Pequeño (Módulo comp izq der) else Base Contenedor 


-- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar laNave peligros = foldr impactar laNave peligros


-- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego peligros  = foldr (\unaNave acumulador -> if sobrevive peligros unaNave then unaNave : acumulador else acumulador) []  

sobrevive :: [Peligro] -> NaveEspacial -> Bool
sobrevive peligros unaNave = puedeVolar $ maniobrar unaNave peligros


-- Ejercicio 8.1
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel _                0 = 1
componentesPorNivel (Base c)         _ = 0
componentesPorNivel (Módulo c n1 n2) x = (componentesPorNivel n1 (x-1)) + (componentesPorNivel n2 (x-1))
--componentesPorNivel laNave n = foldr (\x acc -> (foldNave (\unaNave -> if n > x then 0 + acc else 1 + acc) (\compNave acumIzq acumDer -> if n > x then acumIzq + acumDer else 1 + acc) 0 laNave)) 0 [1..]
--componentesPorNivel laNave = foldr fLoca (\n -> 1)
  --where fLoca = (\n rec -> foldNave (\compBase -> if n == 0 then 1 else 0)(\compNave izq der -> if n == 0 then 1 else (izq n-1) + (der n-1)) laNave)


-- Ejercicio 8.2
dimensiones :: NaveEspacial -> (Int, Int)
dimensiones laNave = (largoNave, anchoNave)
  where largoNave = largo laNave
        anchoNave = maximum (anchosHasta laNave largoNave)

largo :: NaveEspacial -> Int
largo = foldNave (\naveDeUnComponente -> 1) (\compNave acumIzq acumDer-> 1 + max acumIzq acumDer) --(\x recs -> 1 + maximum recs)

anchosHasta :: NaveEspacial -> Int -> [Int]
anchosHasta laNave n = foldr (\elementoLista acc -> (componentesPorNivel laNave elementoLista) : acc) [] [0..n]
