import Text.Show.Functions ()

data Ninja = Ninja {
  nombre       :: String,
  herramientas :: [Herramienta],
  jutsus       :: [Jutsu],
  rango        :: Int
} deriving Show

type Herramienta = (String, Int)

mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango unaFuncion uneNinja = uneNinja { rango = max 0 . unaFuncion . rango $ uneNinja }

mapCantidad :: (Int -> Int) -> Herramienta -> Herramienta
mapCantidad unaFuncion (unNombre, unaCantidad) = (unNombre, unaFuncion unaCantidad)

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas unaFuncion uneNinja = uneNinja { herramientas = unaFuncion . herramientas $ uneNinja }

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

-- A.

-- A.a.

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta unaHerramienta uneNinja = mapHerramientas ((mapCantidad . min . cuantasHerramientasPuedeObtener) uneNinja unaHerramienta :) uneNinja

cuantasHerramientasPuedeObtener :: Ninja -> Int
cuantasHerramientasPuedeObtener = (100 -) . cantidadDeHerramientas

cantidadDeHerramientas :: Ninja -> Int
cantidadDeHerramientas = sum . map snd . herramientas

-- A.b.

usarHerramienta :: String -> Ninja -> Ninja
usarHerramienta unNombreDeHerramienta uneNinja = mapHerramientas (filter ((/= unNombreDeHerramienta) . nombreHerramienta)) uneNinja

-- B

data Mision = Mision {
  cantidadDeNinjas :: Int,
  rangoRecomendado :: Int,
  ninjasEnemigos   :: [Ninja],
  recompensa       :: Herramienta
} deriving Show

type Equipo = [Ninja]

mapCantidadDeNinjas :: (Int -> Int) -> Mision -> Mision
mapCantidadDeNinjas unaFuncion unaMision = unaMision { cantidadDeNinjas = max 1 . unaFuncion . cantidadDeNinjas $ unaMision }

mapNinjasEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
mapNinjasEnemigos unaFuncion unaMision = unaMision { ninjasEnemigos = unaFuncion . ninjasEnemigos $ unaMision }

-- B.a.

esDesafiante :: Equipo -> Mision -> Bool
esDesafiante unEquipo unaMision = tieneMiembroNoCalificadoPara unEquipo unaMision && ((>= 2) . length . ninjasEnemigos) unaMision

tieneMiembroNoCalificadoPara :: Equipo -> Mision -> Bool
tieneMiembroNoCalificadoPara unEquipo unaMision = any (not . estaCalificadoPara unaMision) unEquipo

estaCalificadoPara :: Mision -> Ninja -> Bool
estaCalificadoPara unaMision uneNinja = rango uneNinja >= rangoRecomendado unaMision

-- B.b.

esCopada :: Mision -> Bool
esCopada = esRecompensaCopada . recompensa

esRecompensaCopada :: Herramienta -> Bool
esRecompensaCopada unaHerramienta = elem unaHerramienta recompensasCopadas

recompensasCopadas :: [Herramienta]
recompensasCopadas = [("Bomba de Humo", 3), ("Shuriken", 5), ("Kunai", 14)]

-- B.1.c.

esFactible :: Equipo -> Mision -> Bool
esFactible unEquipo unaMision = (not . esDesafiante unEquipo) unaMision && estaBienPreparadoPara unEquipo unaMision

estaBienPreparadoPara :: Equipo -> Mision -> Bool
estaBienPreparadoPara unEquipo unaMision = tieneSuficientesNinjasPara unEquipo unaMision || estanBienArmades unEquipo

tieneSuficientesNinjasPara :: Equipo -> Mision -> Bool
tieneSuficientesNinjasPara unEquipo unaMision = length unEquipo >= cantidadDeNinjas unaMision

estanBienArmades :: Equipo -> Bool
estanBienArmades = (> 500) . sum . map cantidadDeHerramientas

-- B.2.a.

fallarMision :: Mision -> Equipo -> Equipo
fallarMision = filter . estaCalificadoPara

-- B.2.b.

cumplirMision :: Mision -> Equipo -> Equipo
cumplirMision unaMision = map (obtenerHerramienta (recompensa unaMision) . promover)

promover :: Ninja -> Ninja
promover = mapRango succ

-- B.3.a.

type Jutsu = Mision -> Mision

clonesDeSombra :: Int -> Jutsu
clonesDeSombra = mapCantidadDeNinjas . subtract

-- B.3.b.

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar = mapNinjasEnemigos (filter ((>= 500) . rango))

ejecutarMision :: Equipo -> Mision -> Equipo
ejecutarMision unEquipo = completarMision unEquipo . usarTodosSusJutsus unEquipo

usarTodosSusJutsus :: Equipo -> Mision -> Mision
usarTodosSusJutsus unEquipo unaMision = foldr ($) unaMision . concatMap jutsus $ unEquipo

completarMision :: Equipo -> Mision -> Equipo
completarMision unEquipo unaMision
  | esCopada unaMision || esFactible unEquipo unaMision = cumplirMision unaMision unEquipo
  | otherwise                                           = fallarMision unaMision unEquipo

-- C.

granGuerraNinja :: Mision
granGuerraNinja = Mision {
  cantidadDeNinjas = 100000,
  rangoRecomendado = 100,
  ninjasEnemigos   = infinitosZetsus,
  recompensa       = ("Honor", 1)
}

infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1..]

zetsu :: Int -> Ninja
zetsu unNumero = Ninja {
  nombre       = "Zetsu " ++ show unNumero,
  rango        = 600,
  jutsus       = [],
  herramientas = []
}

{-
  Si la misión es copada, termina de ejecutar sin problemas y se cumple la misión.
  Si el equipo es finito y la misión no es desafiante porque el equipo no tiene un miembro no calificado, termina sin problemas y se cumple la misión.
  Si el equipo es finito, la misión no es desafiante porque el equipo no tiene un miembro no calificado, y no es factible porque el equipo no está bien preparado, termina sin problemas y se falla la misión.
  En caso contrario, no termina de evaluar, ya sea porque tiene que evaluar la totalidad de la lista de enemigos, o la totalidad del equipo.
-}