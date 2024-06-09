import Text.Show.Functions 

data Jugador= UnJugador{
    nombre ::String,
    edad :: Int,
    promedio :: Float,
    habilidad :: Int,
    cansancio ::Float
} deriving Show

martin :: Jugador
martin = UnJugador "Martin" 26 0.0 50 35.0
juan :: Jugador
juan = UnJugador "Juancho" 30 0.2 50 40.0
maxi ::Jugador
maxi = UnJugador "Maxi Lopez" 27 0.4 68 30.0
jonathan :: Jugador
jonathan = UnJugador "Chueco" 20 1.5 80 99.0
lean ::Jugador
lean = UnJugador "Hacha" 23 0.01 50 35.0
brian :: Jugador
brian = UnJugador "Panadero" 21 5 80 15.0
garcia ::Jugador
garcia = UnJugador "Sargento" 30 1 80 13.0
messi ::Jugador
messi = UnJugador "Pulga" 26 10 99 43.0
aguero ::Jugador
aguero = UnJugador "Aguero" 24 5 90 5.0


type Equipo = (NombreEquipo , Grupo , Jugadores)

type NombreEquipo = String
type Grupo = Char
type Jugadores = [Jugador]

equipo1 :: Equipo
equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre :: Equipo
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo ::Equipo
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])


quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs 


{-
Todos saben que un equipo lleva muchos jugadores al mundial pero, ¿Quiénes son las figuras de cada equipo? Dado un equipo
específico queremos saber quiénes son figura, para lo que se requiere tener una habilidad mayor a 75 y promedio de gol mayor a 0.
-}
figuras :: Equipo->[Jugador]
figuras (_,_,jugadores) = filter esFigura jugadores

habilidadMayorA :: Int->Jugador->Bool
habilidadMayorA unNumero unJugador = habilidad unJugador > unNumero

promedioMayorA :: Float->Jugador->Bool
promedioMayorA unPromedio unJugador = promedio unJugador > unPromedio

esFigura :: Jugador -> Bool
esFigura unJugador = habilidadMayorA 75 unJugador && promedioMayorA 0 unJugador

{-
Crear la función tieneFarandulero, la cual verifica si un determinado equipo cuenta entre sus integrantes algún jugador farandulero. 
Los jugadores que son faranduleros están dados por la siguiente función::
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]
-}
obtenerGrupoDeUnEquipo :: Equipo->Grupo
obtenerGrupoDeUnEquipo (_,grupo,_) = grupo

obtenerJugadoresEquipo :: Equipo->[Jugador]
obtenerJugadoresEquipo (_,_,unosJugadores) = unosJugadores

jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

esFarandulero :: Jugador->Bool
esFarandulero unJugador = elem (nombre unJugador) jugadoresFaranduleros

tieneFarandulero :: Equipo->Bool
tieneFarandulero (_,_, jugadores)= any esFarandulero jugadores


{-
 Naturalmente, en todo mundial existe un álbum de figuritas de la compañía Panini. Esta vez se nos pidió que dados una serie de 
 equipos y un grupo específico (A,B,C,D,E o F), le digamos los nombres de los jugadores que tendrían que ser las figuritas 
 difíciles (y hasta a veces brillantes).

Para cumplir la condición de ser difícil, el jugador tiene que cumplir simultáneamente:
Ser figura
Ser joven (menor a 27 años)
No ser farandulero.
-}

esDelGrupo :: Equipo->Grupo->Bool
esDelGrupo (_,grupoEquipo,_) unGrupo  = grupoEquipo == unGrupo

filtradoSegunGrupo :: Char -> [Equipo] -> [Equipo]
filtradoSegunGrupo grupoEspecifico = filter (\equipo-> ((== grupoEspecifico).obtenerGrupoDeUnEquipo) equipo)

esDificil :: Jugador->Bool
esDificil unJugador = esFigura unJugador && esJoven unJugador && not (esFarandulero  unJugador)

esJoven :: Jugador->Bool
esJoven unJugador = edad unJugador <27


figuritasDificiles :: Char -> [Equipo] -> Jugadores
figuritasDificiles grupoEspecifico = obtenerJovenesNoFaranduleros.obtenerFiguras.filtradoSegunGrupo grupoEspecifico

 
obtenerFiguras :: [Equipo] -> Jugadores
obtenerFiguras = concat.map figuras

obtenerJovenesNoFaranduleros :: Jugadores -> Jugadores
obtenerJovenesNoFaranduleros = filter (\unJugador -> esJoven unJugador && (not.esFarandulero) unJugador) 


{-
Se conoce que tras un partido del mundial, los jugadores se cansan debido al extenso calendario que tuvieron en el año y a 
que muchos ya están prontos a su retiro. Definir la función jugarPartido, la cual dado un equpo modifique a sus jugadores según
 los siguientes criterios:

Si el jugador no es farandulero, es joven y figura, su cansancio pasa a ser 50.

Para el resto de los jugadores jóvenes, su cansancio aumenta un 10%.

Si el jugador no es joven y es figura del equipo se incrementa en 20 unidades su cansancio.

En cualquier otro caso, el cansancio se duplica.
-}
modificarCansancio :: (Float->Float)->Jugador->Jugador
modificarCansancio unaFuncion unJugador = unJugador { cansancio = unaFuncion. cansancio $ unJugador }

modificarCansancioLuegoDePartido :: Jugador->Jugador
modificarCansancioLuegoDePartido unJugador
    |esDificil unJugador = modificarCansancio (const 50) unJugador
    | esJoven unJugador = modificarCansancio (+ (0.1 * cansancio unJugador)) unJugador
    | not (esJoven unJugador) && esFigura unJugador = modificarCansancio (+20) unJugador
    |otherwise = modificarCansancio (*2) unJugador

jugarPartido :: Equipo -> Equipo
jugarPartido (nombre,grupo,jugadores) = ( nombre , grupo  , map modificarCansancioLuegoDePartido jugadores  )

{-
 Empezó el mundial y los partidos se empiezan a jugar. ¿Cómo saber quién gana en cada partido? Cuando se enfrentan 2 equipos, 
 se seleccionan los primeros 11 jugadores (por equipo) que menos cansados están y se suma su promedio de gol. El que sume un 
 mejor promedio gana el partido. 
Se pide entonces, dados dos equipos, devolver al ganador del partido, con sus jugadores modificados por haber jugado el partido. 
-}
ganadorPartido :: Equipo->Equipo->Equipo
ganadorPartido unEquipo  =  jugarPartido . quienGana unEquipo 

sumarPromedioDeGol :: Equipo->Float
sumarPromedioDeGol  = sum . map promedio. obtenerJugadoresEquipo 

quienGana :: Equipo->Equipo->Equipo
quienGana unEquipo otroEquipo
    | sumarPromedioDeGol unEquipo > sumarPromedioDeGol otroEquipo = unEquipo
    | otherwise = otroEquipo


seleccionar11MenosCansados :: Equipo->[Jugador]
seleccionar11MenosCansados unEquipo = take 11 (quickSort (\jugador1 jugador2 -> cansancio jugador1 < cansancio jugador2). obtenerJugadoresEquipo $ unEquipo )


{-
Sabiendo ya cómo se decide el ganador de un partido, ahora queremos saber, a partir de un grupo de equipos, qué equipo
 se consagrará campeón del torneo.

¿Cómo se juegan los partidos? 
El primero juega contra el segundo → Ganador1
Ganador1 juega contra tercer equipo → Ganador2
Ganador2 juega contra cuarto equipo → Ganador3

Y así hasta que el ganador del último partido se consagra campeón.
Dar 2 resoluciones diferentes al ejercicio
-}

ganadorDelTorneo :: [Equipo]->Equipo
ganadorDelTorneo = foldl1 ganadorPartido 


campeonDelTorneo' :: [Equipo] -> Equipo
campeonDelTorneo' [cabeza] = cabeza
campeonDelTorneo'  (cabeza1 : cabeza2 : cola) = campeonDelTorneo' (ganadorPartido cabeza1 cabeza2 : cola)


{-
Los días pasaron, las vuvuzelas se escucharon, una nueva Larissa Riquelme se hizo conocida, y el pulpo Paul volvió a acertar 
en los resultados. Después de un gran mundial se quiere saber quién va a ser elegido como el mejor de todos para entregarle 
el premio y ser reconocido en todo el mundo como “EL GROSO”. Para ello se ingresa una lista de equipos, y del equipo elegido 
ganador (el campeón), se quiere saber el nombre del primer jugador que cumpla la condición de ser figura (en todo equipo hay 1 
por lo menos).
-}
elGroso :: [Equipo]->String
elGroso = nombre . primerFigura . ganadorDelTorneo

primerFigura :: Equipo->Jugador
primerFigura  = head . filter esFigura. obtenerJugadoresEquipo



