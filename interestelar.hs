import Text.Show.Functions 

data Planeta = UnPlaneta String Posicion (Tiempo -> Tiempo)

posicion :: Planeta -> Posicion
posicion (UnPlaneta _ p _) = p
tiempo :: Planeta -> (Tiempo ->Tiempo)
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX :: Posicion->Float
coordX (x,_,_) = x
coordY :: Posicion->Float
coordY (_,y,_) = y
coordZ :: Posicion->Float
coordZ (_,_,z) = z

-- De los astronautas sabemos el nombre, la edad terrestre y el planeta en el que están
data Astronauta = UnAstronauta {
  nombre :: String,
  edad :: Tiempo,
  planeta :: Planeta
}

type Velocidad = Float
type Tiempo = Float
type Nave = (Planeta -> Planeta -> Tiempo)

{-
 Saber la distancia entre 2 planetas sabiendo que la distancia entre dos posiciones se calcula de esta forma:

Saber cuánto tiempo se tarda en viajar de un planeta a otro yendo a una determinada velocidad, que es la distancia entre ambos
 dividido por la velocidad de viaje.

-}

--1a
distanciaEntreDosPlanetas :: Planeta -> Planeta -> Float
--qué pasa si agregamos otra coordenada? eso nos ayuda a identificar la repetición de lógica
--distanciaEntreDosPlanetas (UnPlaneta _ (x1, y1, z1) _) (UnPlaneta _ (x2, y2, z2) _) = sqrt(((x1-x2)^2) + ((y1-y2)^2) + ((z1-z2)^2))
--distanciaEntreDosPlanetas (UnPlaneta _ (x1, y1, z1) _) (UnPlaneta _ (x2, y2, z2) _) = sqrt(diferenciaDeCuadrados x1 x2 + diferenciaDeCuadrados y1 y2 + diferenciaDeCuadrados z1 z2)

-- Planteamos una versión distinta basada en orden superior
diferenciaDeCuadrados :: Num a => a -> a -> a
diferenciaDeCuadrados a b = (a - b)^2
coordenadas :: Planeta -> [Float]
coordenadas planeta =  map (\funCoord -> (funCoord.posicion) planeta) [coordX, coordY, coordZ]
distanciaEntreDosPlanetas planeta1 planeta2 = (sqrt.sum.zipWith diferenciaDeCuadrados (coordenadas planeta1).coordenadas) planeta2

--1b
tiempoDeViaje :: Velocidad -> Planeta -> Planeta -> Tiempo
tiempoDeViaje velocidad planeta1 planeta2 = distanciaEntreDosPlanetas planeta1 planeta2 / velocidad


{-2
Hacer una función pasarTiempo que haga que un astronauta pase una determinada cantidad de años en su planeta actual. 
Debería aumentar su edad terrestre en la cantidad de tiempo que el planeta indique a partir de los años indicados.
-}
pasarTiempo :: Tiempo -> Astronauta -> Astronauta
pasarTiempo anios astronauta = envejecer ((tiempo.planeta) astronauta anios) astronauta
--tiempo retorna una función!

envejecer :: Tiempo -> Astronauta -> Astronauta
envejecer anios astronauta = astronauta {edad = edad astronauta + anios}

{-3
Queremos que un astronauta pueda viajar a otro planeta usando una nave determinada. Una nave es una función que dados dos 
planetas (origen y destino) retorna el tiempo requerido para viajar entre ellos. En principio nos interesa modelar las 
siguientes naves:

La nave vieja que cuya velocidad es 7 m/s a menos que tenga menos de 6 tanques de oxígeno, en cuyo caso viaja a 10 m/s.

La nave futurista que viaja tan rápido que el tiempo de viaje es despreciable.

Realizar un viaje implica que el astronauta aumente su edad en el tiempo de viaje correspondiente para llegar al destino elegido 
y cambie de planeta al mismo.
-}

viaje :: Nave -> Planeta -> Astronauta -> Astronauta
viaje nave planetaDestino astronauta = (cambiarPlaneta planetaDestino.envejecer (nave (planeta astronauta) planetaDestino)) astronauta

cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta nuevoPlaneta astronauta = astronauta {planeta = nuevoPlaneta}

naveVieja :: Int -> Nave
naveVieja tanquesDeOxigeno planetaOrigen planetaDestino
 |tanquesDeOxigeno <= 6 = tiempoDeViaje 10 planetaOrigen planetaDestino
 |otherwise = tiempoDeViaje 7 planetaOrigen planetaDestino

naveFuturista :: Nave
naveFuturista _ _ = 0

{-4-a
 Hacer que un grupo de astronautas rescate a un astronauta que quedó varado en otro planeta usando una determinada nave. 
 Lo que se espera como resultado de efectuar un rescate es la lista de astronautas luego de que todos los rescatistas 
 viajen en la nave a buscar al astronauta al planeta donde está varado, incorporen a la tripulación al rescatado tras 
 pasar el tiempo correspondiente en ese planeta y luego viajen todos en la misma nave al planeta de donde vinieron los rescatistas.

Se puede asumir que todos venían del mismo planeta origen, y el tiempo que tiene que esperar el astronauta a rescatar es el 
que tarda la nave en ir de un lado a otro.
-}

viajeGrupal :: Nave -> Planeta -> [Astronauta] -> [Astronauta]
viajeGrupal nave destino astronautas = map (viaje nave destino) astronautas

rescatar :: Nave -> [Astronauta] -> Astronauta -> [Astronauta]
rescatar nave rescatistas varado = (viajeGrupal nave origen . levantar (pasarTiempo tiempoDestino varado) . viajeGrupal nave destino) rescatistas
  where
    origen = (planeta.head) rescatistas
    destino = planeta varado
    tiempoDestino = nave origen destino

levantar varado rescatistas = varado : rescatistas

{-4-b
 Hacer una función que permita conocer dado un grupo de astronautas rescatistas, la nave que usan y un grupo de 
 astronautas que quedaron varados en otros planetas, los nombres de los astronautas varados que podrían ser rescatados. 
 Un astronauta puede ser rescatado por los rescatistas si luego del rescate ninguno de los astronautas (incluyendo al rescatado)
  tiene más de 90 años.
-}

puedeSerRescatado :: Nave -> [Astronauta] -> Astronauta -> Bool
puedeSerRescatado nave rescatistas varado = (any demasiadoViejo.rescatar nave rescatistas) varado

demasiadoViejo :: Astronauta -> Bool
demasiadoViejo = (>90).edad

nombresRescatables :: Nave -> [Astronauta] -> [Astronauta] -> [String]
nombresRescatables nave varados rescatistas = (map nombre.filter (puedeSerRescatado nave rescatistas)) varados



