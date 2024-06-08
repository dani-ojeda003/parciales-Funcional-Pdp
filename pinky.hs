import Text.Show.Functions 


data Animal = UnAnimal {
    iq :: Int,
    especie :: String,
    capacidades :: [Capacidad]
} deriving Show

type Transformar = Animal->Animal
type Capacidad = String

{-
inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual
pinkificar: quitarle todas las habilidades que tenía
superpoderes:  le da habilidades nuevas	
En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la habilidad de “hablar”. 
Si no, lo deja como está. 

-}

pinky :: Animal
pinky = UnAnimal 200 "raton" ["llorar","correr"]

modificarIq :: (Int->Int)->Animal->Animal
modificarIq unaFuncion unAnimal = unAnimal {iq = unaFuncion. iq $ unAnimal}

modificarCapacidades :: ([String]->[String])->Animal->Animal
modificarCapacidades unaFuncion unAnimal= unAnimal {capacidades = unaFuncion . capacidades $ unAnimal }


inteligenciaSuperior :: Int->Transformar
inteligenciaSuperior unNumero= modificarIq (+ unNumero)

pinkificar :: Transformar
pinkificar= modificarCapacidades (const [])


superPoderes :: Transformar
superPoderes unAnimal
    | esDeEspecie "elefante" unAnimal = modificarCapacidades (++ ["no tenerle miedo a ratones"]) unAnimal
    | esDeEspecie "raton" unAnimal && iqMayorA 100 unAnimal = modificarCapacidades (++ ["hablar"]) unAnimal 
    |otherwise = unAnimal


type Criterio = Animal->Bool

{-
antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60.

noTanCuerdo: si tiene más de dos habilidades de hacer sonidos pinkiescos. Hacer una  función pinkiesco, que significa que la 
habilidad empieza con “hacer ”, y luego va seguido de una palabra "pinkiesca", es decir, con 4 letras o menos y al menos una vocal. 
Ejemplo:

> pinkiesco “hacer narf”
True

> pinkiesco “hacer asdf”
True
-}

esDeEspecie :: String->Animal->Bool
esDeEspecie unaEspecie unAnimal = especie unAnimal == unaEspecie

iqMayorA :: Int->Animal->Bool
iqMayorA unIq unAnimal= iq unAnimal > unIq

esCapazaDe :: String->[Capacidad]->Bool
esCapazaDe = elem 

antropomorfico :: Criterio
antropomorfico unAnimal = esCapazaDe "hablar" (capacidades unAnimal) && iqMayorA 60 unAnimal



esPalabraPinkiesca :: String->Bool
esPalabraPinkiesca unaPalabra = cantidadDeLetras unaPalabra <=4 && any esVocal unaPalabra

cantidadDeLetras :: String->Int
cantidadDeLetras  = length 

esVocal :: Char -> Bool
esVocal = flip elem ['a', 'e', 'i', 'o', 'u']


pinkiesco :: Capacidad -> Bool
pinkiesco capacidad = ((=="hacer ").(take 6)) capacidad && esPalabraPinkiesca (drop 6 capacidad)

cantidadDeSonidosPinkiescoMayorA :: Int -> [Capacidad] -> Bool
cantidadDeSonidosPinkiescoMayorA cantidad  = (>cantidad).length.filter pinkiesco

noTanCuerdo :: Criterio
noTanCuerdo animal = cantidadDeSonidosPinkiescoMayorA 2 (capacidades animal)



data Experimento = UnExperimento {
    transformaciones :: [Transformar],
    criterio :: Criterio
} deriving Show

{-
Desarollar experimentoExitoso: Dado un experimento y un animal, indica si al aplicar sucesivamente todas las transformaciones 
se cumple el criterio de éxito. 
-}

experimentoExitoso :: Experimento->Criterio->Animal->Bool
experimentoExitoso unExperimento unCriterio = unCriterio . aplicarExperimento  (transformaciones unExperimento)

aplicarExperimento :: [Transformar]->Animal->Animal
aplicarExperimento unasTransformaciones unAnimal= foldr ($) unAnimal  unasTransformaciones


{-
"En un ratón de coeficiente intelectual 17, con habilidades de destruenglonir el mundo y hacer planes desalmados,
 hacer un experimento que consista en pinkificarlo, luego darle inteligencia superior de 10 y por último darle superpoderes. 
 Como criterio de éxito, ver si quedó antropomórfico" 
-}

ratoncito :: Animal
ratoncito = UnAnimal 17 "raton" ["destruenglonir el mundo","hacer planes desalmados"]

ratonExperimental :: Experimento
ratonExperimental = UnExperimento [superPoderes, inteligenciaSuperior 10, pinkificar] antropomorfico

{-
una lista con los coeficientes intelectuales de los animales que entre sus capacidades, luego de efectuar el experimento, 
tengan alguna de las capacidades dadas.

una lista con las especie de los animales que, luego de efectuar el experimento, tengan entre sus capacidades todas las 
capacidades dadas.

una lista con la cantidad de capacidades de todos los animales que, luego de efectuar el experimento, no tengan ninguna de las 
capacidades dadas.
-}

--no se si esta bien

obtenerCapacidades :: [Animal]->[Capacidad]
obtenerCapacidades unosAnimales = map (capacidades unosAnimales) unosAnimales


tieneAlgunaDeLasCapacidades :: [Capacidad] -> Animal -> Bool
tieneAlgunaDeLasCapacidades unasCapacidades animal = any (map (capacidades unAnimal)) $ unasCapacidades

tieneLasCapacidades :: [Capacidad] -> Animal -> Bool
tieneLasCapacidades capacidades animal = all (flip esCapazaDe animal) capacidades

informeIq :: [Animal] -> [Capacidad] -> [Transformar] -> [Int]
informeIq animales capacidades transformaciones = map (iq) (filter (tieneAlgunaDeLasCapacidades capacidades) (map (flip realizarExperimento transformaciones) animales))

informeEspecies :: [Animal] -> [Capacidad] -> [Transformar] -> [String]
informeEspecies animales capacidades transformaciones = map (especie) (filter (tieneLasCapacidades capacidades) (map (flip realizarExperimento transformaciones) animales))

informeCapacidades :: [Animal] -> [Capacidad] -> [Transformar] -> [Int]
informeCapacidades animales capacidadesRequisito transformaciones = map (length.capacidades) (filter (not.tieneAlgunaDeLasCapacidades capacidadesRequisito) (map (flip realizarExperimento transformaciones) animales))

{-
Aparece un nuevo animal que tiene infinitas capacidades. Dar ejemplos de experimentos que se puedan realizar y que no,
 si los hubiera. Justificar conceptualmente. 
-}
-- Si aparece un animal con infinitas capacidades, se lo puede pinkificar, pero no darle super poderes porque la funcion diverge
 



