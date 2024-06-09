import Text.Show.Functions 


data Persona = UnaPersona{
    nombre :: String,
    dinero :: Float,
    suerte :: Int,
    factores :: [Factor]
} deriving Show

type Factor = (String, Valor)
type Valor = Int

nico :: Persona
nico = UnaPersona "Nico" 100 30 [("amuleto", 3), ("manos magicas",100)]
maiu :: Persona
maiu = UnaPersona "Maiu" 100 42 [("inteligencia",55), ("paciencia",50)]

{-
 Conocer la suerte total de una persona. Si no tiene un amuleto, es su suerte normal, si tiene uno, su suerte se multiplica por 
 el valor de ese amuleto.

En general, sólo se considera que una persona tiene un factor si el valor del mismo es mayor a cero. Tener un amuleto de valor 0 
es lo mismo que no tenerlo en absoluto.

> suerteTotal nico
90
> suerteTotal maiu
42
-}

suerteTotal :: Persona->Int
suerteTotal unaPersona = valorAmuleto unaPersona * suerte unaPersona

tieneFactorParticular :: String->Persona->Bool
tieneFactorParticular unObjeto= elem unObjeto. map fst . factores

valorAmuleto :: Persona -> Int
valorAmuleto unaPersona
    | tieneFactorParticular "amuleto" unaPersona =  (snd . amuleto) unaPersona
    | otherwise               = 1


amuleto :: Persona -> Factor
amuleto  = head . filter esUnAmuleto . factores 

esUnAmuleto :: Factor -> Bool
esUnAmuleto = (=="amuleto").fst

{-
Desarrollar el data para el tipo Juego y las funciones ruleta y maquinita sabiendo que un juego se compone por un nombre, una 
función que determina cuánto dinero se ganaría a partir de un monto apostado y una serie de criterios determinantes para ganar.
 Para modelar el dinero usar el tipo Float.

a- La ruleta que se gana 37 veces lo apostado. Para ganar la persona debe tener una suerte total mayor a 80.

b- La maquinita que se basa en un jackpot y lo que se gana es la apuesta más el jackpot. Para ganar se deben cumplir dos c
ondiciones: que la persona tenga una suerte total mayor a 95 y además que tenga paciencia.
-}

data Juego = UnJuego {
    nombreJuego :: String,
    dineroGanado :: Resultado,
    criterios :: [Criterio]
} deriving Show

type Criterio = Persona->Bool
type Resultado = Float->Float

ruleta:: Juego
ruleta = UnJuego "ruleta" (* 37) [suerteTotalMayorA 80]


suerteTotalMayorA :: Int -> Criterio
suerteTotalMayorA unaSuerte = (> unaSuerte) . suerteTotal


maquinita :: Float->Juego
maquinita jackpot = UnJuego "maquinita" (+ jackpot) [suerteTotalMayorA 95 , tienePaciencia ]

tienePaciencia :: Criterio
tienePaciencia = tieneFactorParticular "paciencia"

{-
 Saber si un jugador puede ganar un juego, lo cual sucede si cumple todas las condiciones para ganar ese juego.
-}

ganaElJuego :: Persona -> Juego -> Bool
ganaElJuego unJugador = all (cumpleCriterioParaGanar unJugador) . criterios

cumpleCriterioParaGanar :: Persona->Criterio->Bool
cumpleCriterioParaGanar unaPersona unCriterio = unCriterio unaPersona


{-
Dado un jugador, una apuesta inicial y una lista de juegos, obtener la cantidad total de dinero que puede conseguir esa 
persona con ese monto si apuesta en cada juego lo conseguido en el juego anterior, evitando los juegos en los cuales no pueda ganar.

Si no puede ganar en ningún juego, el resultado sería la apuesta inicial, ya que no apostaría en ninguno.
Resolver de dos formas:

a- con funciones de orden superior, aplicación parcial y composición
b- con recursividad
-}
dineroTotal :: Persona -> Float -> [Juego] -> Float
dineroTotal unaPersona unMontoInicial = foldl (flip ($)) unMontoInicial . map dineroGanado . filter (ganaElJuego unaPersona)


dineroTotalConRecursividad :: Persona -> Float -> [Juego] -> Float
dineroTotalConRecursividad unaPersona unMontoInicial [] = unMontoInicial
dineroTotalConRecursividad unaPersona unMontoInicial (unJuego : restoDeJuegos)
    | ganaElJuego unaPersona unJuego = dineroTotalConRecursividad unaPersona (dineroGanado unJuego unMontoInicial) restoDeJuegos
    | otherwise                      = dineroTotalConRecursividad unaPersona unMontoInicial restoDeJuegos

dineroTotalNico :: Float
dineroTotalNico = dineroTotal nico 400 [ruleta, maquinita 500]

dineroTotalMaiu :: Float
dineroTotalMaiu = dineroTotal maiu 400 [ruleta, maquinita 500]

{-
Dada una lista de jugadores y una lista de juegos, retornar los nombres de los jugadores que no pueden ganar a ningún juego.
-}

nombresPerdedores :: [Persona] -> [Juego] -> [String]
nombresPerdedores unasPersonas  = map nombre . perdedoresDeTodosLosJuegos unasPersonas

perdedoresDeTodosLosJuegos :: [Persona] -> [Juego] -> [Persona]
perdedoresDeTodosLosJuegos unasPersonas unosJuegos = filter (noGananNinguno unosJuegos) unasPersonas

noGananNinguno :: [Juego] -> Persona -> Bool
noGananNinguno unosJuegos unaPersona = all (not . ganaElJuego unaPersona) unosJuegos


{-
Hacer que un jugador apueste una cantidad en un juego, que implica que la persona baje su saldo esa cantidad y luego juegue 
al juego. Si puede ganar en ese juego retorna a la persona con su saldo incrementado en lo que gana en el juego, de lo contrario 
retorna a la persona sin ganar nada.
-}
modificarDinero :: (Float -> Float) -> Persona -> Persona
modificarDinero unaFuncion unJugador = unJugador {dinero = unaFuncion . dinero $ unJugador}


apostar :: Float -> Juego -> Persona -> Persona
apostar unMonto  unJuego = (aumentarDineroSiGana unJuego unMonto . modificarDinero (subtract unMonto))


aumentarDineroSiGana :: Juego -> Float -> Persona -> Persona
aumentarDineroSiGana unJuego unMonto unJugador
    | ganaElJuego unJugador unJuego = aumentarDinero (dineroGanado unJuego unMonto) unJugador
    | otherwise                     = unJugador

aumentarDinero :: Float -> Persona -> Persona
aumentarDinero unValor = modificarDinero (+ unValor)
