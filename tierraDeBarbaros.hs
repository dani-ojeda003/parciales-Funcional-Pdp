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

--Punto 1.1--
espada:: Peso->Barbaro->Barbaro
espada unPeso = modificarFuerzaBarbaro unPeso  

modificarFuerzaBarbaro :: Int->Barbaro->Barbaro
modificarFuerzaBarbaro unPeso unBarbaro= unBarbaro {fuerza= (fuerza unBarbaro) + (unPeso*2)}

modificarNombre :: (String->String)->Barbaro->Barbaro
modificarNombre unaFuncion unBarbaro= unBarbaro {nombre= unaFuncion.nombre$unBarbaro}

--Punto 1.2--
amuletoMistico :: Habilidad->Objeto
amuletoMistico unaHabilidad unBarbaro= modificarHabilidad ((++)[unaHabilidad]) unBarbaro



--1.3--
varitasDefectuosas:: Objeto 
varitasDefectuosas unBarbaro= modificarHabilidad "Magia".vaciarObjetos$ unBarbaro

vaciarObjetos:: Objeto
vaciarObjetos unBarbaro= unBarbaro{objetos=[]}

--1.4--
ardilla :: Objeto
ardilla unBarbaro= unBarbaro


--1.5--
cuerda :: Objeto->Objeto->Objeto
cuerda unObjeto otroObjeto unBarbaro= unObjeto.otroObjeto $ unBarbaro


--2--

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
