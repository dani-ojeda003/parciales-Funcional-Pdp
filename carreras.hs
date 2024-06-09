import Text.Show.Functions 


data Auto = UnAuto{
    color:: Color,
    velocidad :: Int,
    km ::Int
} deriving (Show,Eq)

type Color = String
type Carrera = [Auto]

{-
Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos
 (en valor absoluto) es menor a 10.

Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos (por haber 
recorrido más distancia que los otros).

Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando.
-}

estaCerca :: Auto->Auto->Bool
estaCerca unAuto otroAuto= esAutoDistinto unAuto otroAuto && (distanciaEnValorAbsoluto unAuto otroAuto) < 10

esAutoDistinto :: Auto->Auto->Bool
esAutoDistinto unAuto otroAuto= color unAuto == color otroAuto

distanciaEnValorAbsoluto :: Auto->Auto->Int
distanciaEnValorAbsoluto unAuto otroAuto= abs (km unAuto - km otroAuto)



vaTranquilo :: Auto->[Auto]->Bool
vaTranquilo unAuto unosAutos= (not . any (estaCerca unAuto) $ unosAutos) && vaGanando unAuto unosAutos


vaGanando :: Auto->[Auto]->Bool
vaGanando unAuto = all (leVaGanando unAuto) . filter ( /= unAuto)

leVaGanando :: Auto->Auto->Bool
leVaGanando unAuto otroAuto = km unAuto > km otroAuto

noLeVaGanando :: Auto->Auto->Bool
noLeVaGanando unAuto otroAuto = not (leVaGanando unAuto otroAuto)


puesto :: Auto->Carrera->Int
puesto unAuto = (+ 1) . posicion unAuto

posicion :: Auto->Carrera->Int
posicion unAuto = length . filter (noLeVaGanando unAuto)

{-
Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo indicada, la distancia recorrida
 por el auto debería ser equivalente a la distancia que llevaba recorrida + ese tiempo * la velocidad a la que estaba yendo.


A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que su velocidad final 
sea la resultante de usar dicho modificador con su velocidad actual.

Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se le reste a la 
velocidad actual la cantidad indicada, y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa.
-}


modificarKm :: (Int->Int)->Auto->Auto
modificarKm unaFuncion unAuto = unAuto {km = unaFuncion. km $ unAuto }

correrDuranteXTiempo :: Int->Auto->Auto
correrDuranteXTiempo unTiempo unAuto = modificarKm (const (distanciaTrasXTiempo unTiempo unAuto)) unAuto


distanciaTrasXTiempo :: Int->Auto->Int
distanciaTrasXTiempo unTiempo unAuto = km unAuto + (unTiempo * velocidad unAuto)

modificarVelocidad :: (Int->Int)->Auto->Auto
modificarVelocidad unaFuncion unAuto = unAuto {velocidad= unaFuncion . velocidad $ unAuto }


reducirVelocidad :: Int->Auto->Auto
reducirVelocidad unaVelocidad unAuto = modificarVelocidad (const (velocidadMinima unaVelocidad unAuto)) unAuto

velocidadMinima :: Int->Auto->Int
velocidadMinima unaVelocidad = max 0. subtract unaVelocidad. velocidad


{-
terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50.

miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos 
que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.

jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que 
dura una cantidad limitada de tiempo, el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y 
finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.
-}


type Poder= Auto->Carrera->Carrera


afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista



terremoto :: Poder
terremoto unAuto = afectarALosQueCumplen (estaCerca unAuto) (modificarVelocidad  (subtract 50))


miguelitos :: Int->Poder
miguelitos unaCantidad unAuto= afectarALosQueCumplen (leVaGanando unAuto) (modificarVelocidad (subtract unaCantidad))


jetPack :: Int->Poder
jetPack unTiempo unAuto =  afectarALosQueCumplen (== unAuto) (modificarVelocidad (\ _ -> velocidad unAuto)  . correrDuranteXTiempo unTiempo . modificarVelocidad (*2))


type Evento= Carrera->Carrera

{-
Desarrollar la función:
simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
que permita obtener la tabla de posiciones a partir del estado final de la carrera, el cual se obtiene produciendo cada evento 
uno detrás del otro, partiendo del estado de la carrera recibido.

Desarrollar las siguientes funciones de modo que puedan usarse para generar los eventos que se dan en una carrera:

correnTodos que hace que todos los autos que están participando de la carrera corran durante un tiempo indicado.

usaPowerUp que a partir de un power up y del color del auto que gatilló el poder en cuestión, encuentre el auto correspondiente 
dentro del estado actual de la carrera para usarlo y produzca los efectos esperados para ese power up.

Mostrar un ejemplo de uso de la función simularCarrera con autos de colores rojo, blanco, azul y negro que vayan inicialmente a 
velocidad 120 y su distancia recorrida sea 0, de modo que ocurran los siguientes eventos:

- todos los autos corren durante 30 segundos
- el azul usa el power up de jet pack por 3 segundos
- el blanco usa el power up de terremoto
- todos los autos corren durante 40 segundos
- el blanco usa el power up de miguelitos que reducen la velocidad en 20
- el negro usa el power up de jet pack por 6 segundos
- todos los autos corren durante 10 segundos
-}

simularCarrera :: Carrera -> [Evento] -> [(Int, Color)]
simularCarrera carrera eventos = (tablaDePosiciones . procesarEventos eventos) carrera

tablaDePosiciones :: Carrera -> [(Int, Color)]
tablaDePosiciones carrera 
  = map (entradaDeTabla carrera) carrera

entradaDeTabla :: Carrera -> Auto -> (Int, String)
entradaDeTabla carrera auto = (puesto auto carrera, color auto)

tablaDePosiciones' :: Carrera -> [(Int, String)]
tablaDePosiciones' carrera = zip (map (flip puesto carrera) carrera) (map color carrera)

procesarEventos :: [Evento] -> Carrera -> Carrera
procesarEventos eventos carreraInicial = foldl (\carreraActual evento -> evento carreraActual) carreraInicial eventos

--procesarEventos eventos carreraInicial =
--   foldl (flip ($)) carreraInicial eventos

correnTodos :: Int -> Evento
correnTodos tiempo = map (correrDuranteXTiempo tiempo)

usaPowerUp :: Poder -> Color -> Evento
usaPowerUp powerUp colorBuscado carrera =
    powerUp autoQueGatillaElPoder carrera
    where autoQueGatillaElPoder = find ((== colorBuscado).color) carrera

find :: (c -> Bool) -> [c] -> c
find cond = head . filter cond


{-
ejemploDeUsoSimularCarrera =
    simularCarrera autosDeEjemplo [
        correnTodos 30,
        usaPowerUp (jetPack 3) "azul",
        usaPowerUp terremoto "blanco",
        correnTodos 40,
        usaPowerUp (miguelitos 20) "blanco",
        usaPowerUp (jetPack 6) "negro",
        correnTodos 10
    ]
-}

{-
Si se quisiera agregar un nuevo power up, un misil teledirigido, que para poder activarlo se deba indicar el color del auto al 
que se quiere impactar, ¿la solución actual lo permite o sería necesario cambiar algo de lo desarrollado en los puntos anteriores? 

Si una carrera se conformara por infinitos autos, ¿sería posible usar las funciones del punto 1b y 1c de modo que terminen 
de evaluarse? Justificar.
-}

---- Punto 5
{-
5a

Se puede agregar sin problemas como una función más misilTeledirigido :: Color -> PowerUp, y usarlo como:
usaPowerUp (misilTeledirigido "rojo") "azul" :: Evento

5b

- vaTranquilo puede terminar sólo si el auto indicado no va tranquilo
(en este caso por tener a alguien cerca, si las condiciones estuvieran al revés, 
terminaría si se encuentra alguno al que no le gana).
Esto es gracias a la evaluación perezosa, any es capaz de retornar True si se encuentra alguno que cumpla 
la condición indicada, y all es capaz de retornar False si alguno no cumple la condición correspondiente. 
Sin embargo, no podría terminar si se tratara de un auto que va tranquilo.

- puesto no puede terminar nunca porque hace falta saber cuántos le van ganando, entonces por más 
que se pueda tratar de filtrar el conjunto de autos, nunca se llegaría al final para calcular la longitud.
-}