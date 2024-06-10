import Text.Show.Functions 


type Lugar = (Nombre , Distancia , [Caracteristica])

type Nombre = String
type Distancia = Int
type Caracteristica = String

nombreLugar :: Lugar->String
nombreLugar (nombre, _ ,_)= nombre
distanciaLugar :: Lugar->Int
distanciaLugar (_,km, _)= km
caracteristicasLugar :: Lugar->[String]
caracteristicasLugar (_,_,carac) = carac


{-
Playero: Va a lugares que tengan playa
Mejor Cerca: Va a lugares que no estén a más de 500 kilómetros de distancia.
Gastronómico: Va a lugares que tengan algún producto típico comestible.
 Esquiador: Va a lugares con nieve y montañas y que estén a más de 500 kilómetros de distancia.
-}


lugaresDeEjemplo :: [Lugar]
lugaresDeEjemplo = [("Rosario", 300,["Monumento a la bandera", "Rio"]),("Mar Del Plata", 400, ["Playa", "Alfajores", "Puloveres"]), ("Bariloche",1600, ["Montañas", "Nieve", "Puloveres", "Chocolate"])]

playero :: Lugar->Bool
playero = tiene "playa" 


mejorCerca :: Lugar->Bool
mejorCerca unLugar = distanciaLugar unLugar <= 500

productosComestibles:: [String]
productosComestibles = ["alfajor","milanesa","hamburguesa","empanada","chocolate"]

gastronomico :: Lugar->Bool
gastronomico unLugar = any (esComestible) productosComestibles

esComestible :: String->Bool
esComestible unaCaracteristica = elem unaCaracteristica productosComestibles


esquiador :: Lugar->Bool
esquiador unLugar = tiene "playa" unLugar && tiene "montaña" unLugar && not (mejorCerca unLugar)

tiene :: String->Lugar->Bool
tiene unaAtraccion unLugar = elem unaAtraccion (caracteristicasLugar unLugar)


type Persona = (Nombre , EstiloDeVacaciones)
type EstiloDeVacaciones = Lugar->Bool
juan :: Persona
juan =("Juan", playero)
ana :: Persona
ana = ("Ana", mejorCerca)
jorge :: Persona
jorge = ("Jorge", gastronomico)

{-
Informar los nombres de los lugares a los que puede ir de vacaciones una persona, según su estilo.
puedeIr jorge lugaresDeEjemplo
[“Mar del Plata”, “Bariloche”]
Jorge prefiere el estilo gastronómico, y en mar del plata hay alfajores y en bariloche hay chocolates, mientras
que en Rosario no hay ningún producto típico comestible.

3. Dada una lista de personas, obtener el nombre del lugar más elegido para vacacionar.
lugarMasElegido [juan, ana, jorge] lugaresDeEjemplo
“Mar del Plata”
Son tres los que pueden ir a Mar del plata (está cerca, tiene alfajores y playa) mientras que los otros lugares
son elegidos por menos personas.

4. Dada una lista de personas y un lugar, se quiere saber si todas las personas pueden ir de
vacaciones a ese lugar.
puedenIrTodosA [juan, ana, jorge] (lugaresDeEjemplo !! 1)
True
(Todos pueden ir a Mar del Plata)

5. Una familia quiere irse todos juntos de vacaciones y quiere ver qué lugares encuentra que sean
compatibles para todos. Mostrar ejemplo de invocación y respuesta
-}

--2
puedeIr :: [Lugar]->Persona->[String]
puedeIr unosLugares unaPersona = map nombreLugar . filter (snd unaPersona) $ unosLugares

--3
--lugarMasElegido :: [Persona]->[Lugar]->String
--lugarMasElegido unasPersonas unosLugares = map (nombreLugar ). puedeIr unosLugares unasPersonas

--4
puedeIrAunLugar :: Lugar->Persona->Bool
puedeIrAunLugar unLugar unaPersona = snd unaPersona unLugar

puedenIrTodos :: [Persona]->Lugar->Bool
puedenIrTodos unasPersonas unLugar = all (puedeIrAunLugar unLugar) unasPersonas

--5
familia :: [Persona]->[Lugar]->[Lugar]
familia unasPersonas = filter (puedenIrTodos unasPersonas)