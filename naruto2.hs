import Text.Show.Functions 



data Ninja = UnNinja{
    nombre ::String,
    herramientas :: [Herramienta],
    jutsus :: [Jutsu],
    rango :: Int
} deriving Show

gaara :: Ninja
gaara =  UnNinja "Gaara" [bombasDeHumo, kunais] [] 10

naruto :: Ninja
naruto = UnNinja "Naruto" [bombasDeHumo, kunais] [] 15

suna :: Ninja
suna =  UnNinja "Suna" [bombasDeHumo, kunais] [] 1


bombasDeHumo :: Herramienta
bombasDeHumo = ("Bombas de humo" ,1)
kunais :: Herramienta
kunais =  ("Kunais" ,1)
shurikens :: Herramienta
shurikens =  ("Shurikens" ,1)
sellosExplosivos :: Herramienta
sellosExplosivos = ("Sellos explosivos", 400)


type Herramienta = (NombreHerramienta , Cantidad)
type NombreHerramienta = String
type Cantidad = Int
type Jutsu = Mision->Mision

modificarHerramientas :: ([Herramienta]->[Herramienta])->Ninja->Ninja
modificarHerramientas unaFuncion unNinja = unNinja {herramientas = unaFuncion . herramientas $ unNinja}

cantidadTotalHerramientas :: Ninja->Int
cantidadTotalHerramientas = sum . map snd . herramientas


obtenerHerramienta :: Herramienta->Ninja->Ninja
obtenerHerramienta unaHerramienta unNinja
    | cantidadTotalHerramientas unNinja + snd unaHerramienta <= 100 = modificarHerramientas (++ [unaHerramienta]) unNinja 
    | otherwise = modificarHerramientas (++ [(fst unaHerramienta , 100 - cantidadTotalHerramientas unNinja)]) unNinja


usarHerramienta :: Herramienta->Ninja->Ninja
usarHerramienta unaHerramienta unNinja = modificarHerramientas (const (sacarUnaHerramienta unaHerramienta unNinja)) unNinja

sacarUnaHerramienta :: Herramienta->Ninja->[Herramienta]
sacarUnaHerramienta unaHerramienta  = filter (/= unaHerramienta) . herramientas 



data Mision = UnaMision{
    cantidad :: Int,
    rangoRecomendado :: Int,
    enemigos :: [Ninja],
    recompensa :: Herramienta
} deriving Show

type Equipo = [Ninja]

esDesafiante :: Equipo->Mision->Bool
esDesafiante unEquipo unaMision = hayAlguienConRangoMenor unEquipo unaMision && hayMasDe1Enemigo unaMision


hayMasDe1Enemigo :: Mision->Bool
hayMasDe1Enemigo = (>1) . length . enemigos 

tieneMenorRango :: Mision->Ninja->Bool
tieneMenorRango  unaMision unNinja = rango unNinja < rangoRecomendado unaMision

hayAlguienConRangoMenor :: Equipo->Mision->Bool
hayAlguienConRangoMenor unEquipo unaMision = any (tieneMenorRango unaMision) unEquipo

equipo1 :: Equipo
equipo1 = [ naruto, suna]

matarALosDeLaAldeaDeLaArena :: Mision
matarALosDeLaAldeaDeLaArena = UnaMision 3 10 [ gaara, gaara] bombasDeHumo 

recompensasCopadas :: [Herramienta]
recompensasCopadas = [("bombas de humo", 3) , ("shurikens" , 5) , ("kunais", 14)]

esCopada :: Mision->Bool
esCopada unaMision =  elem (recompensa unaMision) recompensasCopadas

esFactible :: Equipo->Mision->Bool
esFactible unEquipo unaMision = not (esDesafiante unEquipo unaMision ) && (cuentaConCantidadNecesaria unEquipo unaMision || cantidadTotalDeHerramientasEquipo unEquipo > 500 )

cuentaConCantidadNecesaria :: Equipo->Mision->Bool
cuentaConCantidadNecesaria unEquipo unaMision = length unEquipo >= cantidad unaMision

cantidadTotalDeHerramientasEquipo :: Equipo->Int
cantidadTotalDeHerramientasEquipo  = sum . map cantidadTotalHerramientas

estaCalificadoPara :: Mision -> Ninja -> Bool
estaCalificadoPara unaMision unNinja = rango unNinja >= rangoRecomendado unaMision


fallarMision :: Mision->Equipo->Equipo
fallarMision  = filter . estaCalificadoPara 


modificarRango :: (Int->Int)->Ninja->Ninja
modificarRango unaFuncion unNinja = unNinja {rango = unaFuncion. rango $ unNinja}


cumplirMision :: Mision -> Equipo -> Equipo
cumplirMision unaMision  =map (obtenerHerramienta (recompensa unaMision) . modificarRango (+1))

modificarCantidad :: (Int->Int)->Mision->Mision
modificarCantidad unaFuncion unaMision = unaMision {cantidad = unaFuncion . cantidad $ unaMision}

clonesDeSombra :: Int->Mision->Mision
clonesDeSombra unaCantidad unaMision = modificarCantidad (const (max 1 (cantidad unaMision - unaCantidad))) unaMision

modificarEnemigos :: ([Ninja]->[Ninja])->Mision->Mision
modificarEnemigos unaFuncion unaMision = unaMision {enemigos = unaFuncion . enemigos $ unaMision }


fuerzaDeUnCentenar :: Mision->Mision
fuerzaDeUnCentenar = modificarEnemigos (filter ((>= 500) . rango))


ejecutarMision :: Equipo -> Mision -> Equipo
ejecutarMision unEquipo = completarMision unEquipo . usarTodosSusJutsus unEquipo

usarTodosSusJutsus :: Equipo -> Mision -> Mision
usarTodosSusJutsus unEquipo unaMision = foldr ($) unaMision . concatMap jutsus $ unEquipo

completarMision :: Equipo -> Mision -> Equipo
completarMision unEquipo unaMision
  | esCopada unaMision || esFactible unEquipo unaMision = cumplirMision unaMision unEquipo
  | otherwise                                           = fallarMision unaMision unEquipo




granGuerraNinja :: Mision
granGuerraNinja = UnaMision 100000 100 infinitosEnemigos abanicoMadara


abanicoMadara :: Herramienta
abanicoMadara = ("abanico de Madara", 1)


zetsu :: Int->Ninja
zetsu unNumero = UnNinja {
    nombre = "zetsu" ++ show unNumero,
    herramientas = [],
    jutsus = [],
    rango = 600
}

infinitosEnemigos :: [Ninja]
infinitosEnemigos = map zetsu [1..]

{-
  Si la misión es copada, termina de ejecutar sin problemas y se cumple la misión.

  Si el equipo es finito y la misión no es desafiante porque el equipo no tiene un miembro no calificado, termina sin problemas
   y se cumple la misión.

  Si el equipo es finito, la misión no es desafiante porque el equipo no tiene un miembro no calificado, y no es factible porque 
  el equipo no está bien preparado, termina sin problemas y se falla la misión.

  En caso contrario, no termina de evaluar, ya sea porque tiene que evaluar la totalidad de la lista de enemigos, o la totalidad
   del equipo.
-}
