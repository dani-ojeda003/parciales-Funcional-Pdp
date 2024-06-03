import Text.Show.Functions
import Data.List


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


--Punto 1.2--
amuletoMistico :: Habilidad->Objeto
amuletoMistico unaHabilidad= modificarHabilidad unaHabilidad


modificarHabilidad :: Habilidad->Objeto
modificarHabilidad unaHabilidad unBarbaro= unBarbaro {habilidades= habilidades unBarbaro ++ [unaHabilidad] }


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

--megafono :: Objeto
--megafono unBarbaro= 


--megafonoBarbarico :: Objeto
--megafonoBarbarico unBarbaro= cuerda.ardilla.megafono $ unBarbaro

--3--
type Aventuras = Barbaro->Bool
--invasionDeSuciosDuendes :: Aventura


--cremalleraDelTiempo :: Aventura
--cremalleraDelTiempo= barbaroSobrevive

barbaroSobrevive :: Barbaro->Bool
barbaroSobrevive unaAventura unBarbaro= sabeEscribirPoesiaAtroz unBarbaro || barbaroSinPulgares unBarbaro  || ritualDeFechorias unBarbaro

sabeEscribirPoesiaAtroz:: Barbaro->Bool
sabeEscribirPoesiaAtroz unBarbaro= elem "Escribir Poesia Atroz" (habilidades unBarbaro)

barbaroSinPulgares :: Barbaro->Bool
barbaroSinPulgares unBarbaro= nombre unBarbaro == "Faffy" || nombre unBarbaro == "Astro"

--ritualDeFechorias ::