import Text.Show.Functions





data Participante=UnParticipante{
    nombre:: String,
    trucosCocina :: [Truco],
    especialidad:: Plato
} deriving Show

data Plato= UnPlato{
    dificultad::Int,
    ingredientes:: [Ingrediente]
} deriving Show

type Ingrediente= (NombreIngrediente , Peso)
type NombreIngrediente= String
type Peso=Int

{-
endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.  
salar: la vieja y confiable… dada una cantidad de gramos de sal y un plato, nos retorna el mismo con esa cantidad de sal para que quede flama.
darSabor: dadas una cantidad de sal y una de azúcar sala y endulza un plato.
duplicarPorcion: se duplica la cantidad de cada componente de un plato… para más placer.
simplificar: si un plato tiene más de 5 componentes y una dificultad mayor a 7 lo vamos a simplificar, sino lo dejamos igual. Simplificar un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes de los que hayamos agregado menos de 10 gramos.


esVegano: si no tiene carne, huevos o alimentos lácteos.
esSinTacc: si no tiene harina.
esComplejo: cuando tiene más de 5 componentes y una dificultad mayor a 7.
noAptoHipertension: si tiene más de 2 gramos de sal.
-}


duplicarCantidadDelComponente :: Ingrediente->Ingrediente
duplicarCantidadDelComponente (ingrediente, cantidad) = (ingrediente, cantidad *2)

type Truco= Plato->Plato

modificarTrucoDeCocina:: ([Truco]->[Truco])->Participante->Participante
modificarTrucoDeCocina unaFuncion unParticipante= unParticipante {trucosCocina= unaFuncion.trucosCocina$ unParticipante}

modificarIngredientes :: ([Ingrediente]->[Ingrediente])->Plato->Plato
modificarIngredientes unaFuncion unPlato= unPlato{ingredientes=unaFuncion.ingredientes$unPlato}

agregarIngrediente :: Ingrediente-> Plato -> Plato
agregarIngrediente unIngrediente unPlato = unPlato{ingredientes = (unIngrediente : ) . ingredientes $ unPlato}



endulzar :: Int -> Truco
endulzar cantidad = agregarIngrediente ("Azucar",cantidad)

salar :: Int->Truco
salar cantidad = agregarIngrediente ("Sal",cantidad)

darSabor :: Int->Int->Truco
darSabor cantidadAzucar cantidadSal= endulzar cantidadAzucar.salar cantidadSal

duplicarPorcion :: Truco
duplicarPorcion = modificarIngredientes  (map duplicarCantidadDelComponente)


simplificar:: Truco
simplificar unPlato
    | esComplejo unPlato = modificarDificultad (const 5).modificarIngredientes (filter ((>10). snd) ) $ unPlato
    |otherwise= unPlato


esComplejo :: Plato->Bool
esComplejo unPlato= cantidadDeIngredientes unPlato > 5 && dificultad unPlato > 7

cantidadDeIngredientes:: Plato->Int
cantidadDeIngredientes= length.ingredientes

modificarDificultad::(Int->Int)->Plato->Plato
modificarDificultad unaFuncion unPlato= unPlato {dificultad= unaFuncion . dificultad $ unPlato}


esVegano:: Plato->Bool
esVegano unPlato= not (contiene "carne" unPlato) && not (contiene "huevos" unPlato) && not (contiene "lacteos" unPlato)

esSinTacc :: Plato->Bool
esSinTacc unPlato= not (contiene "harina" unPlato)

contiene :: NombreIngrediente->Plato->Bool
contiene unNombre unPlato = elem unNombre  (map fst . ingredientes$unPlato)


noAptoHipertension :: Plato->Bool
noAptoHipertension unPlato= contiene "sal" unPlato && cantidadDe "sal" unPlato> 2


cantidadDe :: NombreIngrediente -> Plato -> Int
cantidadDe nombreIngrediente = snd . obtenerComponenteCon nombreIngrediente . ingredientes

obtenerComponenteCon :: NombreIngrediente -> [Ingrediente] -> Ingrediente
obtenerComponenteCon nombreIngrediente = head . filter (es nombreIngrediente)

es :: NombreIngrediente -> Ingrediente -> Bool
es nombreIngrediente unIngrediente = nombreIngrediente == fst unIngrediente


pepe ::Participante
pepe = UnParticipante "Pepe Ronccino" [salar 2, endulzar 5,simplificar,duplicarPorcion] platoDePepe


platoDePepe:: Plato
platoDePepe= UnPlato 8 [("sal",10)]

{-
cocinar: es el momento en el que la magia ocurre y vemos como queda finalmente el plato de un participante luego de aplicar todos sus trucos a su especialidad.
esMejorQue: en esta contienda diremos que un plato es mejor que otro si tiene más dificultad pero la suma de los pesos de sus componentes es menor.
participanteEstrella  diremos que la estrella es quien luego de que todo el grupo cocine tiene el mejor plato.
-}


--foldr :: (b->a->a) -> a -> [b] -> a
cocinar :: Participante -> Plato
cocinar unParticipante = aplicarTrucos (especialidad unParticipante) (trucosCocina unParticipante)

aplicarTrucos :: Plato -> [Truco] -> Plato
aplicarTrucos = foldr ($)


esMejorQue:: Participante->Participante->Bool
esMejorQue unParticipante otroParticipante= tieneMasDificultad unParticipante otroParticipante && sumaDePesos unParticipante otroParticipante

tieneMasDificultad :: Participante->Participante->Bool
tieneMasDificultad unParticipante otroParticipante= (dificultad.especialidad $ unParticipante) > (dificultad . especialidad $ otroParticipante)

sumaDePesos :: Participante->Participante->Bool
sumaDePesos unParticipante otroParticipante= (sum. map snd. ingredientes.especialidad $ unParticipante) > (sum. map snd. ingredientes.especialidad $ otroParticipante)


--participanteEstrella:: [Participante]->Participante
--participanteEstrella (cabeza1:[])= cabeza1
--participanteEstrella (cabeza1:cabeza2:cola)= esMejorQue cabeza1 cabeza2 && participanteEstrella (cabeza2:cola)

--Para finalizar vamos a modelar el plato definitivo, el platinum. Este plato tiene de especial que tiene infinitos componentes misteriosos con cantidades incrementales y dificultad 10:--

planitum ::Plato
planitum = UnPlato 10 infinitos


infinitos:: [Ingrediente]
infinitos= map compMist [1..]

compMist :: Int->Ingrediente
compMist unNumero =("Ingrediente " ++ show unNumero, unNumero)

