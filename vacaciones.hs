import Text.Show.Functions

{-
Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 
Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
Paseo en barco: depende de cómo esté la marea
si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
si está moderada, no pasa nada.
si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.

-}

data Turista = UnTurista{
    cansancio ::Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving Show

type Excursion= Turista->Turista

type Marea= String

ana ::Turista
ana = UnTurista 0 21 False ["espanol"]

beto ::Turista
beto = UnTurista 15 15 True ["aleman"]

cathi ::Turista
cathi = UnTurista 15 15 True ["aleman","catalan"]

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = modificarCansancio (subtract 5) unTurista
    | otherwise = modificarStress (subtract 1) unTurista

modificarCansancio:: (Int->Int)->Turista->Turista
modificarCansancio unaFuncion unTurista= unTurista {cansancio= unaFuncion . cansancio $ unTurista}

modificarStress:: (Int->Int)->Turista->Turista
modificarStress unaFuncion unTurista= unTurista {stress = unaFuncion. stress $ unTurista}

modificarIdiomas :: ([String]->[String])->Turista->Turista
modificarIdiomas unaFuncion unTurista = unTurista {idiomas= unaFuncion. idiomas $ unTurista }

modificarViajaSolo :: (Bool->Bool)->Turista->Turista
modificarViajaSolo unaFuncion unTurista= unTurista {viajaSolo= unaFuncion. viajaSolo $ unTurista }


apreciarElementoDelPaisaje :: String->Excursion
apreciarElementoDelPaisaje unElemento= modificarStress (subtract (cantidadDeLetrasDeUnElemento unElemento))

cantidadDeLetrasDeUnElemento:: String->Int
cantidadDeLetrasDeUnElemento = length 

salirAHablarIdiomaEspecifico :: String->Excursion
salirAHablarIdiomaEspecifico unIdioma = modificarIdiomas (++ [unIdioma]). modificarViajaSolo (const False)


caminar :: Int->Excursion
caminar unosMinutos= modificarCansancio (+ (intensidad unosMinutos)).modificarStress (subtract (intensidad unosMinutos))


intensidad ::Int->Int
intensidad unosMinutos= div unosMinutos 4

paseoEnBarco:: Marea->Excursion
paseoEnBarco unaMarea unTurista
    | unaMarea == "fuerte" = modificarCansancio (+ 10) . modificarStress (+ 10) $ unTurista
    | unaMarea == "tranquila" = caminar 10 . apreciarElementoDelPaisaje "mar" . salirAHablarIdiomaEspecifico "aleman" $ unTurista
    | otherwise = unTurista

{-
Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.
Dada la función: 
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.

Usar la función anterior para resolver cada uno de estos puntos:
Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.

-}


hacerExcursion :: Excursion->Turista->Turista
hacerExcursion unaExcursion = cambiarStressPorcentual (10). unaExcursion 

cambiarStressPorcentual :: Int->Turista->Turista
cambiarStressPorcentual unPorcentaje unTurista = modificarStress (subtract (div (unPorcentaje * stress unTurista) 100)) unTurista


deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun f turista excursion = deltaSegun f (hacerExcursion excursion turista) turista



esEducativa :: Turista -> Excursion -> Bool
esEducativa turista = (> 0) . deltaExcursionSegun (length . idiomas) turista 

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista


type Tour = [Excursion]

{-
Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, y finaliza con una salida con gente local que habla "melmacquiano".
Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra isla: si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.
Modelar los tours para:
Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades como cantidad de excursiones tenga el tour, y luego realizar las excursiones en orden.
Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla.
Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour. 
La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.
-}

completo :: Tour
completo = [caminar 20, apreciarElementoDelPaisaje "cascada", caminar 40, irALaPlaya, salidaLocal ]

ladoB ::Excursion->Tour
ladoB excursion = [paseoEnBarco "tranquila", excursion, caminar 120 ]

islaVecina :: Marea->Tour
islaVecina unaMarea = [paseoEnBarco unaMarea, excursionEnIslaVecina unaMarea,paseoEnBarco unaMarea]


excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina unaMarea
    | unaMarea== "fuerte" = apreciarElementoDelPaisaje "lago"
    | otherwise = irALaPlaya

salidaLocal :: Excursion
salidaLocal = salirAHablarIdiomaEspecifico "melmacquiano"

--a--

hacerTour :: Turista -> Tour -> Turista
hacerTour unTurista unTour = foldl (flip hacerExcursion) (modificarStress ( + length unTour) unTurista) unTour

-- b)
propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (dejaAcompaniado turista) . excursionesDesestresantes turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . viajaSolo . flip hacerExcursion turista

-- c)
efectividad :: Tour -> [Turista] -> Int
efectividad tour = sum . map (espiritualidadAportada tour) . filter (flip esConvincente tour)

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour turista =
  deltaSegun nivelDeRutina (hacerTour turista tour) turista

nivelDeRutina :: Turista -> Int
nivelDeRutina turista = cansancio turista + stress turista


-- 4)
-- a)
playasEternas :: Tour
playasEternas = salidaLocal : repeat irALaPlaya

-- b)
{-
Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
-}

-- c)
{-
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.
-}
