module Library where
import PdePreludat

-- Parcial Functional Master Series

-- Nombre: Benito, Ariel
-- Legajo: 163316-8

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre
type RimaPalabra =  Palabra -> Palabra -> Bool

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"
 
tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

--Palabras 

sonIguales ::  Palabra -> Palabra -> Bool
sonIguales palabra1 palabra2 = palabra1 == palabra2

rimaAsonante :: RimaPalabra
rimaAsonante palabra1 palabra2 = last(filter esVocal palabra1) == last(filter esVocal palabra2) || last(filter tieneTilde palabra1) == last(filter tieneTilde palabra2)

rimaConsonante :: RimaPalabra
rimaConsonante palabra1 palabra2 = take 3 (reverse palabra1) == take 3 (reverse palabra2)

esRima :: RimaPalabra
esRima palabra1 palabra2 
    | sonIguales palabra1 palabra2 = False
    | rimaAsonante palabra1 palabra2 = True
    | rimaConsonante palabra1 palabra2 = True
    | otherwise = False
    

--Versos

type RimaVerso = Verso->Verso->Bool

ultimaPalabra :: Verso->Palabra
ultimaPalabra = last.words

primerPalabra :: Verso->Palabra
primerPalabra  = head.words

porMedioDeRimas :: RimaVerso
porMedioDeRimas verso1 verso2 = esRima (ultimaPalabra verso2) (ultimaPalabra verso1)

anadiplosis :: RimaVerso
anadiplosis verso1 verso2 = ultimaPalabra verso2 == primerPalabra verso1

versosRiman :: RimaVerso
versosRiman verso1 verso2 
    | anadiplosis verso1 verso2 = True
    | otherwise = False


--Patrones

type Patron = Estrofa -> Bool
--aca entendi que vamos a evaluar si 2 versos en una posicion especifica que nosotros aclaramos por parametro cumplen con el patron simple x y

patronSimple :: Number->Number->Patron
patronSimple posicion1 posicion2 estrofa  = versosRiman (tomarVerso posicion1 estrofa) (tomarVerso posicion2 estrofa)

tomarVerso :: Number ->Estrofa-> Verso
tomarVerso numero estrofa = estrofa!!(numero-1)

esVocalPrima :: Char -> Bool
esVocalPrima = flip elem "áéíóúaeiou"

palabraEsEsdrujula :: Palabra -> Bool
palabraEsEsdrujula palabra = tieneTilde(reverse(filter esVocalPrima palabra)!!2)

versoTerminaEnEsdrujula :: Verso -> Bool
versoTerminaEnEsdrujula  = palabraEsEsdrujula.ultimaPalabra

patronEsdrujula :: Patron
patronEsdrujula  = all versoTerminaEnEsdrujula 

listaDePrimerasPalabras :: Estrofa -> [String]
listaDePrimerasPalabras = map primerPalabra 

primerPalabraEstrofa :: Estrofa -> String
primerPalabraEstrofa  = head.listaDePrimerasPalabras 

patronAnafora :: Patron
patronAnafora estrofa = all (== primerPalabraEstrofa estrofa)(listaDePrimerasPalabras estrofa)

patronCadena :: Patron
patronCadena estrofa =(...)

patronCombinaDos ::  Patron ->Patron ->Estrofa ->Bool
patronCombinaDos   patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

patronaabb :: Patron
patronaabb  = patronCombinaDos (patronSimple 1 2) (patronSimple 3 4)

patronabab :: Patron
patronabab  = patronCombinaDos  (patronSimple 1 3) (patronSimple 2 4)

patronabba :: Patron
patronabba  = patronCombinaDos  (patronSimple 1 4) (patronSimple 2 3)

patronhardcore :: Patron
patronhardcore  = patronCombinaDos  (patronCadena) (patronEsdrujula)

{-Patrones
-¿Se podría saber si una estrofa con infinitos versos cumple con el patrón hardcore? 
No se puede saber por que nunca terminaria de ejecutarse ya que el patron cadena analisa si cada verso 
cumple un determinado patron con el siguiente, entonces nunca terminaria de analisar ya que la estrofa es 
infinita

-¿Y el aabb? 
Este si se puede saber ya que al usar el patron simple solo analisa los versos que le especificamos 
(1 con 2 y 3 con 4) y gracias a que Haskkel usa LazyEvaluation, aunque la estrofa sea infinita una vez
que chequea que se cumple esta condicion termina la ejecucion.
-}


--Puestas en escena

data PuestaEnEscena = PuestaEnEscena {
    publicoExaltado :: Bool,
    potencia :: Number,
    estrofa :: Estrofa,
    artista :: Artista
} deriving (Show,Eq)

type Tecnica = PuestaEnEscena -> PuestaEnEscena

aumentarPotencia :: Number->PuestaEnEscena -> PuestaEnEscena
aumentarPotencia numero puesta  = puesta {potencia = (potencia puesta)*numero}

tecnicaGritar :: Tecnica
tecnicaGritar = aumentarPotencia 1.5 

exaltarPublico :: PuestaEnEscena -> PuestaEnEscena
exaltarPublico puesta = puesta {publicoExaltado = True}

tecnicaResponderUnAcote :: Bool -> Tecnica
tecnicaResponderUnAcote efectividad  
    | efectividad == True = ((aumentarPotencia 1.2) . (exaltarPublico)) 
    | otherwise = (aumentarPotencia  1.2 )

cumplePatron :: Patron -> PuestaEnEscena -> Bool
cumplePatron patron puesta = patron (estrofa puesta)

tecnicaTirarTecnicas :: Patron -> Tecnica
tecnicaTirarTecnicas patron puesta 
    | cumplePatron patron puesta == True = ((aumentarPotencia 1.1) . (exaltarPublico)) puesta
    | otherwise = aumentarPotencia 1.1 puesta

tirarFreestyle :: Artista ->Tecnica-> Estrofa->PuestaEnEscena
tirarFreestyle artista tecnica estrofa = tecnica (PuestaEnEscena{ publicoExaltado = False, potencia = 1, estrofa= estrofa, artista= artista})

-- Jurados

type Jurado = PuestaEnEscena ->Number

type CriterioJurado = PuestaEnEscena ->Number

cumpleConaabb :: CriterioJurado
cumpleConaabb puesta 
    | patronaabb( estrofa puesta) = 0.5
    | otherwise = 0

patronEsdujulasYSimple14:: Patron
patronEsdujulasYSimple14  = patronCombinaDos  (patronSimple 1 4) (patronEsdrujula)

cumpleConEsdujulasYSimple14 :: CriterioJurado
cumpleConEsdujulasYSimple14 puesta 
    | patronEsdujulasYSimple14( estrofa puesta) = 1
    |  otherwise = 0

publicoExaltadoPrima :: CriterioJurado
publicoExaltadoPrima puesta 
    | publicoExaltado puesta = 1
    | otherwise = 0

potenciaMayorA1con5 :: CriterioJurado
potenciaMayorA1con5 puesta 
    | potencia puesta > 1.5 = 2
    | otherwise = 0

juradoAlToke :: Jurado
juradoAlToke puesta = cumpleConaabb puesta + cumpleConEsdujulasYSimple14 puesta + publicoExaltadoPrima puesta + potenciaMayorA1con5 puesta

--juradoGenerico :: PuestaEnEscena->[CriterioJurado]-> Number
--juradoGenerico puesta criterios = sum (map puesta criterios)

--puntaje :: PuestaEnEscena ->[CriterioJurado] -> Number
--puntaje  puesta criterios = max 3 (jurado puesta criterios) 