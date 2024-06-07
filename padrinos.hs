import Text.Show.Functions 

data Chico=UnChico{
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} deriving Show

type Deseo = Chico->Chico
type Habilidad = String

timmy :: Chico
timmy= UnChico "timmy" 10 ["mirar tele", "jugar a la pc"] [serMayor]

{-
aprenderHabilidades habilidades unChico : agrega una lista de habilidades
nuevas a las que ya tiene el chico.

serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades
de jugar a todas las versiones pasadas y futuras del Need For Speed, que
son: “jugar need for speed 1”, “jugar need for speed 2”, etc.

serMayor unChico: Hace que el chico tenga 18 años.
-}

aprenderHabilidades :: [Habilidad]->Deseo
aprenderHabilidades unasHabilidades= modificarHabilidades (++ unasHabilidades)

modificarHabilidades :: ([String]->[String])->Chico->Chico
modificarHabilidades unaFuncion unChico = unChico {habilidades= unaFuncion. habilidades $ unChico}


serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed = modificarHabilidades (++ jugarNeedForSpeed)


jugarNeedForSpeed :: [Habilidad]
jugarNeedForSpeed = map needForSpeed [1..]

needForSpeed :: Int->Habilidad
needForSpeed unNumero = "Jugar Need For Speed " ++ show unNumero


serMayor :: Deseo
serMayor= modificarEdad (const 18)


modificarEdad :: (Int->Int)->Chico->Chico
modificarEdad unaFuncion unChico = unChico {edad= unaFuncion. edad $ unChico}

{-
wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar
(crecer un año de edad).

 cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de
edad. Como es olvidadizo, no le concede ningún deseo.

 muffinMagico: dado un chico le concede todos sus deseos.
-}


type Padrino = Chico->Chico

wanda :: Padrino
wanda unChico = madurar . aplicarDeseo (obtenerPrimerDeseo unChico) $ unChico


madurar :: Chico->Chico
madurar = modificarEdad (+1)

obtenerPrimerDeseo :: Chico->Deseo
obtenerPrimerDeseo unChico = head (deseos unChico)

aplicarDeseo :: Deseo->Chico->Chico
aplicarDeseo unDeseo = unDeseo


cosmo :: Padrino
cosmo = desMadurar

desMadurar :: Chico->Chico
desMadurar = modificarEdad (`div` 2)

--Va de atras a adelante--
--muffinMagico :: Chico->Chico
--muffinMagico unChico = foldr ($) unChico (deseos unChico)

muffinMagico :: Chico -> Chico
muffinMagico unChico = foldl (cumplirDeseo) unChico (deseos unChico)

cumplirDeseo :: Chico -> Deseo -> Chico
cumplirDeseo  = flip ($)

{-
tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice
si la posee.

esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más
de 18 años) y además sabe manejar.
-}


tieneHabilidad :: Habilidad->Chico->Bool
tieneHabilidad unaHabilidad  = elem unaHabilidad.habilidades 

esSuperMaduro :: Chico->Bool
esSuperMaduro unChico = edad unChico >= 18 && tieneHabilidad "manejar" unChico


type Condicion = Chico->Bool

data Chica = UnaChica{
    nombreChica :: String,
    condicion :: Condicion
}

trixie :: Chica
trixie = UnaChica "trixie" noEsTimmy

vicky:: Chica
vicky = UnaChica "vicky" (tieneHabilidad "superModelo Noruego")

noEsTimmy :: Chico->Bool
noEsTimmy unChico = nombre unChico /= "timmy"

{-
quienConquistaA unaChica losPretendientes: Dada una chica y una lista
de pretendientes, devuelve al que se queda con la chica, es decir, el primero
que cumpla con la condición que ella quiere. Si no hay ninguno que la cumpla,
devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este
punto se puede usar recursividad)
-}


quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica  = elPrimeroQueCumpla (condicion unaChica)

elPrimeroQueCumpla :: Condicion -> [Chico] -> Chico
elPrimeroQueCumpla _ [unChico] = unChico
elPrimeroQueCumpla condicion (chico1 : cola)
    | condicion chico1 = chico1
    | otherwise         = elPrimeroQueCumpla condicion cola



jessica :: Chica
jessica = UnaChica "jessica" (tieneHabilidad "cocinar")

{-
infractoresDeDaRules : Dada una lista de
chicos, devuelve la lista de los nombres de
aquellos que tienen deseos prohibidos. Un deseo
está prohibido si, al aplicarlo, entre las
cinco primeras habilidades, hay alguna prohibida.
En tanto, son habilidades prohibidas enamorar,
matar y dominar el mundo.
-}


habilidadesProhibidas :: [Habilidad]
habilidadesProhibidas = ["matar", "enamorar", "dominar el mundo"]



esHabilidadProhibida::Habilidad -> Bool
esHabilidadProhibida unaHabilidad = elem unaHabilidad habilidadesProhibidas


esDeseoProhibido::Chico -> Deseo -> Bool
esDeseoProhibido unChico unDeseo = any esHabilidadProhibida.take 5 .habilidades . unDeseo $ unChico


tieneDeseoProhibido::Chico -> Bool
tieneDeseoProhibido unChico = any (esDeseoProhibido unChico).deseos $ unChico

infractoresDeDaRules::[Chico] -> [Chico]
infractoresDeDaRules = filter tieneDeseoProhibido

