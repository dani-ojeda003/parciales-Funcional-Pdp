import Text.Show.Functions 



data Autor = UnAutor{
    obras :: [Obra],
    nombre :: String
} deriving Show

type Obra = (String,Int)
a :: Obra
a = ("Habia una vez un pato",1997)
b :: Obra
b = ("¡Habia una vez un pato!", 1996)
c:: Obra
c = ("Mirtha, Susana y Moria", 2010)
d :: Obra
d= ("La semantica funcional del amoblamiento vertebral es riboficiente", 2020)
e :: Obra
e = ("La semantica funcional de Mirtha, Susana y Moria", 2022)

autorA ::Autor
autorA = UnAutor [a] "A"
autorB ::Autor
autorB = UnAutor [b] "B"
autorC ::Autor
autorC = UnAutor [c] "C"
autorD ::Autor
autorD = UnAutor [d] "D"
autorE ::Autor
autorE = UnAutor [e] "E"

{-
Conocer la versión cruda de un texto, que consiste en eliminar los acentos de las letras existentes y quitar signos de puntuación y 
todo carácter que no sea una letra o un número. 

Por ejemplo, la versión cruda de "Había una vez un pato..." es "Habia una vez un pato"
-}


versionCruda::String ->String
versionCruda =filter esLetraONumero.map sinAcento

esLetraONumero:: Char->Bool
esLetraONumero unCaracter = elem unCaracter todasLasLetrasYNumeros 

sinAcento::Char->Char
sinAcento 'á'='a'
sinAcento 'é'='e'
sinAcento 'í'='i'
sinAcento 'ó'='o'
sinAcento 'ú'='u'
sinAcento letra=letra

todasLasLetrasYNumeros :: [Char]
todasLasLetrasYNumeros = ['a'..'z']++['A'..'Z'] ++ "0123456789"


{-
Copia literal: ocurre cuando la versión cruda de una es igual a la de la otra. Por ejemplo, A es plagio de B. 

Empieza igual: Los primeros caracteres de una obra son los mismos que otra, y su longitud es menor. La cantidad de caracteres a 
analizar puede ser variable. Por ejemplo, E es plagio de D para una cantidad 10, pero no para una cantidad 30.

Le agregaron intro: La obra plagiada empieza a su manera, pero al final incluye totalmente el texto de la original. Por ejemplo, E 
es plagio de C.

Inventar otra forma de detectar plagio, utilizando una expresión lambda.
-}
type Plagio = Obra->Obra->Bool

copiaLiteral :: Obra->Obra->Bool
copiaLiteral (texto1, _) (texto2, _)= versionCruda texto1 == versionCruda texto2


empiezaIgual :: Int->Obra->Obra->Bool 
empiezaIgual unaCantidad unaObra posiblePlagio = tienenLosPrimerosNCaracteresIguales unaCantidad unaObra posiblePlagio && cantidadDeCaracteres unaObra <cantidadDeCaracteres posiblePlagio


tienenLosPrimerosNCaracteresIguales :: Int->Obra->Obra->Bool
tienenLosPrimerosNCaracteresIguales unaCantidad unaObra posiblePlagio = tomarNPrimerosCaracteres unaCantidad unaObra == tomarNPrimerosCaracteres unaCantidad posiblePlagio


tomarNPrimerosCaracteres :: Int->Obra->String
tomarNPrimerosCaracteres n (texto, _) = take n texto

cantidadDeCaracteres :: Obra->Int
cantidadDeCaracteres (texto, _)= length texto


leAgregaronIntro :: Obra->Obra->Bool
leAgregaronIntro unaCopia unaObra = ultimosElementosCopia (length unaObra)  unaCopia == fst unaObra


ultimosElementosCopia :: Int->Obra->String
ultimosElementosCopia cant (texto, _) = drop (length texto - cant) texto


{-
Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.

Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. Es decir, el segundo plagió al primero, 
el tercero al segundo, y así. Se considera que un autor plagió a otro cuando alguna de sus obras es plagio de alguna de las del 
otro según el bot.

Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  que significa que luego 
de que el bot detectara que una de sus obras fue plagio de alguna de los otros autores, nunca más volvió a plagiar. 
En definitiva, su plagio detectado fue el primero y el último.
-}

data Bot= UnBot{
    deteccion:: [Plagio],
    fabricante :: String
} deriving (Eq,Show)


puedeDetectarPlagio :: Plagio->Obra->Obra->Bool
puedeDetectarPlagio forma obra obraOriginal = forma obra obraOriginal


--esCadenaDePlagiadores :: [Autor]->Bot->Bool
--esCadenaDePlagiadores


cadenaPlagiadores :: Bot ->  [Autor] -> Bool
cadenaPlagiadores bot [ _]  = False
cadenaPlagiadores bot [x1,x2]  = plagioA bot x1 x2
cadenaPlagiadores bot (x1:x2:xs)  = plagioA bot x1 x2 && cadenaPlagiadores bot (x2:xs)

plagioA :: Bot ->  Autor-> Autor -> Bool
plagioA bot autor autorOriginal = any   (esPlagioDeEsteAutor  bot autorOriginal) (obras autor)

esPlagioDeEstaAutor:: Bot -> Autor-> Obra -> Bool
esPlagioDeEstaAutor bot autorOriginal obra = any   (esPlagioDeEstaObra bot obra)   (obras autorOriginal)
