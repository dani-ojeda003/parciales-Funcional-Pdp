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
receta :: Pais->Pais
receta unPais= prestarNMillones (200).explotacion "Mineria"$ unPais


puedenZafar :: [Pais]->[Pais]
puedenZafar unosPaises= filter (elem "Petroleo".recursosNaturales) unosPaises

totalDeuda :: [Pais]->Int
totalDeuda unosPaises= sum.map deuda $ unosPaises

--5
estaOrdenado:: Pais-> [Receta]->Bool
estaOrdenado unPais _ = True
estaOrdenado unPais (receta1:receta2:cola)= pbi (receta1 unPais) < pbi (receta2 unPais) && estaOrdenado unPais (receta2:cola) 