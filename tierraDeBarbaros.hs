import Text.Show.Functions
import Data.List
import Data.Char (toUpper)


type Peso= Int
type Habilidad= String
type Objeto=Barbaro->Barbaro

data Barbaro= UnBarbaro{
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
} deriving (Show)

dave :: Barbaro
dave = UnBarbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas ]

--Punto 1.1 Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.--
espada:: Peso->Barbaro->Barbaro
espada unPeso = modificarFuerzaBarbaro unPeso  

modificarFuerzaBarbaro :: Int->Barbaro->Barbaro
modificarFuerzaBarbaro unPeso unBarbaro= unBarbaro {fuerza= (fuerza unBarbaro) + (unPeso*2)}

modificarNombre :: (String->String)->Barbaro->Barbaro
modificarNombre unaFuncion unBarbaro= unBarbaro {nombre= unaFuncion.nombre$unBarbaro}

--Punto 1.2 Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.--
amuletoMistico :: Habilidad->Objeto
amuletoMistico unaHabilidad unBarbaro= modificarHabilidad ((++)[unaHabilidad]) unBarbaro



--1.3 Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.--
varitasDefectuosas:: Objeto 
varitasDefectuosas unBarbaro= modificarHabilidad "Magia".vaciarObjetos$ unBarbaro

vaciarObjetos:: Objeto
vaciarObjetos unBarbaro= unBarbaro{objetos=[]}

--1.4 Una ardilla, que no hace nada.--
ardilla :: Objeto
ardilla unBarbaro= unBarbaro


--1.5  Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. --
cuerda :: Objeto->Objeto->Objeto
cuerda unObjeto otroObjeto unBarbaro= unObjeto.otroObjeto $ unBarbaro


--2  El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas. Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, que está formado por una cuerda, una ardilla y un megáfono. --

megafono :: Objeto
megafono unBarbaro= modificarHabilidad (ponerMayusculas.concatenar) unBarbaro


ponerMayusculas:: [String]->[String]
ponerMayusculas unasHabilidades= map (map toUpper) unasHabilidades

concatenar:: [String]->[String]
concatenar unasHabilidades=  [concat unasHabilidades]



modificarHabilidad :: ([String]->[String])->Barbaro->Barbaro
modificarHabilidad unaFuncion unBarbaro= unBarbaro{habilidades= unaFuncion.habilidades$unBarbaro}



megafonoBarbarico :: Objeto
megafonoBarbarico unBarbaro= cuerda ardilla megafono $ unBarbaro

--3-- 
{-

Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal, pero ahora que tienen nuestra ayuda, quieren que se les diga si un grupo de bárbaros puede sobrevivir a cierta aventura.  Una aventura se compone de uno o más eventos, por ejemplo:

invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”
cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí. 
ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 
saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.
caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus habilidades contienen más de 3 vocales y comienzan con mayúscula.

Sabiendo esto, se pide:
Definir los eventos, modelar las aventuras y dar un ejemplo. 
Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)




-}
type Aventura = Barbaro->Bool

invasionDeSuciosDuendes :: Aventura
invasionDeSuciosDuendes = barbaroSobrevive

cremalleraDelTiempo :: Aventura
cremalleraDelTiempo= barbaroSobrevive

barbaroSobrevive :: Aventura
barbaroSobrevive unBarbaro= sabeEscribirPoesiaAtroz unBarbaro || barbaroSinPulgares unBarbaro  || ritualDeFechorias unBarbaro

sabeEscribirPoesiaAtroz:: Aventura
sabeEscribirPoesiaAtroz unBarbaro= elem "Escribir Poesia Atroz" (habilidades unBarbaro)

barbaroSinPulgares :: Aventura
barbaroSinPulgares unBarbaro= nombre unBarbaro == "Faffy" || nombre unBarbaro == "Astro"



saqueo :: Barbaro->Bool
saqueo unBarbaro= elem "robar" (habilidades unBarbaro) && (fuerza unBarbaro>80)

gritoGuerra:: Barbaro->Bool
gritoGuerra unBarbaro = poderGritoDeGuerra unBarbaro > cantidadDeLetrasHabilidades unBarbaro

poderGritoDeGuerra:: Barbaro->Int
gritoDeGuerra =(*4).length.objetos

cantidadDeLetrasHabilidades :: Barbaro->Int
cantidadDeLetrasHabilidades = sum . map length . habilidades


caligrafia :: Barbaro->Bool
caligrafia unBarbaro= all tieneMasDe3VocalesYEmpiezaConMayuscula (habilidades unBarbaro)

tieneMasDe3VocalesYEmpiezaConMayuscula:: String->Bool
tieneMasDe3VocalesYEmpiezaConMayuscula unaHabilidad =  tieneMasDe3Vocales unaHabilidad && empiezaConMayuscula unaHabilidad

tieneMasDe3Vocales:: String->Bool
tieneMasDe3Vocales unaHabilidad= (>3).length.filter esVocal $ unaHabilidad

esVocal:: Char->Bool
esVocal 'a'=True
esVocal 'e'=True
esVocal 'i'=True
esVocal 'o'=True
esVocal 'u'=True
esVocal _= False

--isUpper dice si empieza con mayuscula--

empiezaConMayuscula:: String->Bool
empiezaConMayuscula unaHabilidad= isUpper.head$unaHabilidad


ritualDeFechorias :: [Aventura]->Aventura
ritualDeFechorias enventos unBarbaro= pasaUnaAventura any unBarbaro eventos


sobrevivientes :: [Barbaro]->Aventura->[Barbaro]
sobrevivientes unosBarbaros unaAventura= filter (\ unBarbaro-> pasaUnaAventura all unBarbaro unaAventura ) unosBarbaros

pasaUnaAventura criterio unBarbaro naAventura = criterio (\evento-> evento unBarbaro) unaAventura
{- 
A - Los bárbaros se marean cuando tienen varias habilidades iguales. Por todo esto, nos piden desarrollar una función que elimine los elementos repetidos de una lista (sin utilizar nub ni nubBy)

> sinRepetidos [1,2,3,4,4,5,5,6,7]
[1,2,3,4,5,6,7]

Nota: Puede usarse recursividad para este punto.

B - Los bárbaros son una raza muy orgullosa, tanto que quieren saber cómo van a ser sus descendientes y asegurarse de que los mismos reciban su legado.

El descendiente de un bárbaro comparte su nombre, y un asterisco por cada generación. Por ejemplo "Dave*", "Dave**" , "Dave***" , etc. 

Además, tienen en principio su mismo poder, habilidades sin repetidos, y los objetos de su padre, pero antes de pasar a la siguiente generación, utilizan (aplican sobre sí mismos) los objetos. Por ejemplo, el hijo de Dave será equivalente a:

(ardilla.varitasDefectuosas) (Barbaro "Dave*" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas])


Definir la función descendientes, que dado un bárbaro nos de sus infinitos descendientes. 

-}

sinRepetidos []= []
sinRepetidos (cabeza:cola)
    | elem cabeza cola= cola
    |otherwise= (cabeza:cola)

descendiente :: Barbaro->Barbaro
descendiente unBarbaro= tilizarObjetos.modificarNombre ( ++ "*" ) .modificarHabilidad sinRepetidos$unBarbaro


utilizarObjetos ::Barbaro->Barbaro
utilizarObjetos unBarbaro= foldr ($) unBarbaro (objetos unBarbaro)


descendientes:: Barbaro->[Barbaro]
descendientes unBarbaro= iterate descendiente unBarbaro 
