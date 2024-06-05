import Text.Show.Functions


data Pais=UnPais {
    ingresoPerCapita::Int,
    sectorPublico:: Int,
    sectorPrivado ::Int,
    recursosNaturales::[Recurso],
    deuda :: Int
} deriving Show

type Recurso=String
type Receta= Pais->Pais

namibia :: Pais
namibia =UnPais 4140 400000 650000 ["Mineria","EcoTurismo"] 50
{-
prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)
reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario
darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.
establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.

-}


--1
prestarNMillones :: Int->Receta
prestarNMillones cantPrestamo unPais= modificarDeuda (+ obtenerPorcentaje 150 unPais)  unPais

modificarDeuda :: (Int->Int)->Pais->Pais
modificarDeuda unaFuncion unPais=unPais {deuda= unaFuncion.deuda $ unPais}

obtenerPorcentaje :: Int->Pais->Int
obtenerPorcentaje unPorcentaje unPais= (div(unPorcentaje*(deuda unPais)) 100)

--2

--reducirXCantidadSector :: Int->Receta
--reducirXCantidadSector unaCantidad unPais
  --  |sectorPublico unPais > 100 = modificarSectorPublico (subtract unaCantidad).modificarIngreso (subtract (obtenerPorcentaje 20)) unPais
   -- |otherwise = modificarSectorPublico (subtract unaCantidad).modificarIngreso (subtract (obtenerPorcentaje 15) unPais


modificarIngreso :: (Int->Int)->Pais->Pais
modificarIngreso unaFuncion unPais = unPais{ingresoPerCapita= unaFuncion .ingresoPerCapita$unPais}

modificarSectorPublico :: (Int->Int)->Pais->Pais
modificarSectorPublico unaFuncion unPais= unPais{sectorPublico= unaFuncion .sectorPublico$unPais}



explotacion :: Recurso->Receta
explotacion unRecurso unPais= modificarDeuda (subtract 2).modificarRecursoNatural (filter (/= unRecurso)) $ unPais

modificarRecursoNatural :: ([String]->[String])->Pais->Pais
modificarRecursoNatural unaFuncion unPais= unPais {recursosNaturales= unaFuncion.recursosNaturales $ unPais}




blindaje :: Receta
blindaje unPais= modificarDeuda (+ (div (pbi unPais) 2)).modificarSectorPublico (subtract 500) $ unPais

pbi :: Pais->Int
pbi unPais= (ingresoPerCapita unPais)*(sectorPublico unPais + sectorPrivado unPais)

--3

{-
Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.
Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.
-}
receta :: Pais->Pais
receta unPais= prestarNMillones (200).explotacion "Mineria"$ unPais

{-
Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.
Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
Indicar en dónde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver el requerimiento.

-}

puedenZafar :: [Pais]->[Pais]
puedenZafar unosPaises= filter (elem "Petroleo".recursosNaturales) unosPaises

totalDeuda :: [Pais]->Int
totalDeuda unosPaises= sum.map deuda $ unosPaises

--5
{-
dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor.
Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per cápita por la población activa (privada y pública). 
-}
estaOrdenado:: Pais-> [Receta]->Bool
estaOrdenado unPais _ = True
estaOrdenado unPais (receta1:receta2:cola)= pbi (receta1 unPais) < pbi (receta2 unPais) && estaOrdenado unPais (receta2:cola) 
