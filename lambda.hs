
import Text.Show.Functions 

data Depto = UnDepto {
    ambientes :: Int,
    superficie :: Int, 
    precio :: Int,
    barrio :: String
} deriving Show

{-
Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa 
función sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.

Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando
 ordenarSegun.
-}

ordenarSegun:: (a->a->Bool)->[a]->[a]
ordenarSegun _ [] = []
ordenarSegun anteriorA (x:xs)=
    ordenadosQueCumplen anteriorA (`anteriorA` x) xs 
    ++ [x] ++
    ordenadosQueCumplen anteriorA (not.(`anteriorA` x)) xs
        where ordenadosQueCumplen anteriorA condicion  = ordenarSegun anteriorA.filter condicion
-- la funcion ordenadosQueCumplen usaba point free todo el tiempo

between:: Ord a => a -> a -> a -> Bool
between x y z = 
    x<=z && y >=z

deptosDeEjemplo = [
    UnDepto 3 80 7500 "Palermo",
    UnDepto 1 45 3500 "Villa Urquiza",
    UnDepto 2 50 5000 "Palermo",
    UnDepto 1 45 5500 "Recoleta"]

--1)
--a
mayor::Ord b=>(a->b)->a->a->Bool
mayor funcion valor1 valor2 =
    funcion valor1 > funcion valor2

menor::Ord b=>(a->b)->a->a->Bool
menor funcion valor1 valor2 =
        funcion valor1 < funcion valor2
    

--b

    --ordenarSegun:: (a->a->Bool)->[a]->[a]
    --ordenarSegun (mayor length) ["holaa","chau"]

--2)
{-
ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en 
alguno de los barrios de la lista.

cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el 
departamento se encuentra entre los dos valores indicados
-}

ubicadoEn::[String] -> Depto -> Bool
ubicadoEn barrios departamento = 
    any (estaEn departamento) barrios

estaEn departamento elBarrio =
    barrio departamento == elBarrio

ubicadoEn' barrios =
   -- elem (barrio departamento) barrios
    flip elem barrios.barrio

--b
    --between:: Ord a => a -> a -> a -> Bool
cumpleRango::Ord a => (Depto->a)->a->a-> Depto -> Bool 
cumpleRango funcion cotaInf cotaSup =
    between cotaInf cotaSup .funcion
    --between cotaInf cotaSup (funcion depto)

--3)
{-
Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.

Definir la función departamentosBuscadosEnOrdenDeInteres que a partir de una búsqueda, un criterio de ordenamiento y una lista de 
departamentos retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido.

Mostrar un ejemplo de uso de buscar para obtener los departamentos que cumplan con:
Se encuentren en Recoleta o Palermo 

Sean de 1 a 2 ambientes 

Que se alquilen a menos de $6000 por mes

De modo que el resultado se encuentre ordenado por mayor superficie con la lista de departamentos de ejemplo dada.
-}

--a
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

--cumpleBusqueda:: [Requisito] -> Depto -> Bool
cumpleBusqueda:: Busqueda -> Depto -> Bool

cumpleBusqueda requisitos departamento =
    all (cumpleRequisito departamento) requisitos 

cumpleRequisito departamento requisito = requisito departamento

--cumpleBusqueda [ubicadoEn'["Palermo","San Miguel"] ,cumpleRango precio 1000 7000 ] departamentito


--b

type CriterioDeOrd = Depto->Depto->Bool

departamentosBuscadosEnOrdenDeInteres::  Busqueda -> CriterioDeOrd -> [Depto] -> [Depto]
departamentosBuscadosEnOrdenDeInteres requisitos criterioDeOrdenamiento =
        ordenarSegun criterioDeOrdenamiento
          .filter (cumpleBusqueda requisitos)

--c

consulta3c deptosDeEjemplo =
    departamentosBuscadosEnOrdenDeInteres 
    [ubicadoEn'["Recoleta","Palermo"],
        cumpleRango ambientes 1 2,
        (<6000).precio]
    (mayor superficie)
    deptosDeEjemplo

--4)
{-
Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los mails de las 
personas que tienen alguna búsqueda que se cumpla para el departamento dado.
-}
data Persona = UnaPersona {
    mail::String,
    busquedas::[Busqueda]
}
mailsDePersonasInteresadas:: Depto -> [Persona] -> [String]
mailsDePersonasInteresadas departamento  =
    map mail.filter (leInteresa departamento) 

leInteresa::Depto -> Persona -> Bool
leInteresa departamento =
    any (flip cumpleBusqueda departamento).busquedas 

--5) :t f te da la solucion
{-
Inferir el tipo de la siguiente función:
f x y = y.head.map (\(_,z) -> menor x z).filter (even.fst)
-}

f x y = y.head.map(\(_,z)-> menor x z).filter (even.fst)