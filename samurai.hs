import Text.Show.Functions 


data Elemento = UnElemento {
    tipo :: String,
    ataque :: Transformacion,
    defensa :: Transformacion 
} deriving Show

data Personaje = UnPersonaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
} deriving Show

dani ::Personaje
dani= UnPersonaje "dani" 500 [maldad] 2024

maldad ::Elemento
maldad=UnElemento "Maldad" (mandarAlAnio 10) meditar

type Transformacion = Personaje->Personaje

{-
mandarAlAnio: lleva al personaje al año indicado.
meditar: le agrega la mitad del valor que tiene a la salud del personaje.
causarDanio: le baja a un personaje una cantidad de salud dada.
Hay que tener en cuenta al modificar la salud de un personaje que ésta nunca puede quedar menor a 0.
-}

modificarAnio :: (Int->Int)->Personaje->Personaje
modificarAnio unaFuncion unPersonaje = unPersonaje {anioPresente = unaFuncion. anioPresente $ unPersonaje}

mandarAlAnio :: Int->Transformacion
mandarAlAnio unAnio= modificarAnio (const unAnio)

modificarSalud :: (Float->Float)->Personaje->Personaje
modificarSalud unaFuncion unPersonaje= unPersonaje {salud= unaFuncion . salud $ unPersonaje }

meditar :: Transformacion
meditar unPersonaje = modificarSalud (+ calcularMitadDeSalud unPersonaje) unPersonaje

calcularMitadDeSalud :: Personaje->Float
calcularMitadDeSalud unPersonaje =  salud unPersonaje / 2


causarDanio :: Float->Transformacion
causarDanio unDanio unPersonaje = modificarSalud (const $ saludLuegoDeAtaque unDanio unPersonaje) unPersonaje

{-
esMalvado, que retorna verdadero si alguno de los elementos que tiene el personaje en cuestión es de tipo “Maldad”.
danioQueProduce :: Personaje -> Elemento -> Float, que retorne la diferencia entre la salud inicial del personaje y la salud del 
personaje luego de usar el ataque del elemento sobre él.
enemigosMortales que dado un personaje y una lista de enemigos, devuelve la lista de los enemigos que pueden llegar a matarlo con un 
solo elemento. Esto sucede si luego de aplicar el efecto de ataque del elemento, el personaje queda con salud igual a 0.
-}

saludLuegoDeAtaque :: Float->Personaje->Float
saludLuegoDeAtaque unDanio unPersonaje= max 0 (salud . modificarSalud (subtract unDanio) $ unPersonaje) 


esMalvado :: Personaje->Bool
esMalvado  = elem "Maldad". map tipo . elementos 


danioQueProduce :: Personaje->Elemento->Float
danioQueProduce unPersonaje unElemento = salud unPersonaje - salud (ataque unElemento unPersonaje)


enemigosMortales :: Personaje->[Personaje]->[Personaje]
enemigosMortales unPersonaje = filter (esEnemigoMortal unPersonaje)


esEnemigoMortal :: Personaje->Personaje->Bool
esEnemigoMortal unPersonaje= any (tieneAtaqueMortal unPersonaje) . elementos


tieneAtaqueMortal :: Personaje->Elemento->Bool
tieneAtaqueMortal personaje elemento = estaMuerto . ataque elemento $ personaje


estaMuerto :: Personaje->Bool
estaMuerto = (==0).salud

{-
Definir concentracion de modo que se pueda obtener un elemento cuyo efecto defensivo sea aplicar meditar tantas veces como el nivel 
de concentración indicado y cuyo tipo sea "Magia".

Definir esbirrosMalvados que recibe una cantidad y retorna una lista con esa cantidad de esbirros (que son elementos de tipo “Maldad”
 cuyo efecto ofensivo es causar un punto de daño).

Definir jack de modo que permita obtener un personaje que tiene 300 de salud, que tiene como elementos concentración nivel 3 y una
 katana mágica (de tipo "Magia" cuyo efecto ofensivo es causar 1000 puntos de daño) y vive en el año 200.

Definir aku :: Int -> Float -> Personaje que recibe el año en el que vive y la cantidad de salud con la que debe ser construido. 
Los elementos que tiene dependerán en parte de dicho año. Los mismos incluyen:
Concentración nivel 4
Tantos esbirros malvados como 100 veces el año en el que se encuentra.
Un portal al futuro, de tipo “Magia” cuyo ataque es enviar al personaje al futuro (donde el futuro es 2800 años después del año 
indicado para aku), y su defensa genera un nuevo aku para el año futuro correspondiente que mantenga la salud que tenga el personaje al usar el portal.

-}
noHacerNada :: Transformacion
noHacerNada = id

concentracion :: Int->Elemento
concentracion nivelConcentracion = UnElemento {
    tipo= "Magia",
    ataque= noHacerNada,
    defensa = foldr1 (.) (replicate nivelConcentracion meditar)
} 

esbirro :: Elemento
esbirro = UnElemento "Maldad" (modificarSalud (subtract 1)) noHacerNada


esbirrosMalvados :: Int->[Elemento]
esbirrosMalvados unaCantidad = replicate unaCantidad esbirro


katanaMagica :: Elemento
katanaMagica = UnElemento "Magia" (modificarSalud (subtract 1000)) noHacerNada

jack:: Personaje
jack = UnPersonaje "Jack" 300 [concentracion 3, katanaMagica] 200

aku :: Int -> Float -> Personaje
aku anio saludInicial = UnPersonaje {
  nombre = "Aku",
  salud = saludInicial,
  anioPresente = anio,
  elementos = concentracion 4 : portalFuturo anio : esbirrosMalvados (100 * anio)
}


portalFuturo :: Int -> Elemento
portalFuturo anio = UnElemento "Magia" (mandarAlAnio (anioFuturo anio)) (aku anioFuturo.salud)

anioFuturo :: Int->Int
anioFuturo anio = anio + 2800


{-
Finalmente queremos saber cómo puede concluir la lucha entre Jack y Aku. Para ello hay que definir la función 
luchar donde se espera que si el primer personaje (el atacante) está muerto, 
retorne la tupla con el defensor primero y el atacante después, en caso contrario la lucha continuará invirtiéndose los papeles 
(el atacante será el próximo defensor)luego de que ambos personajes se vean afectados por el uso de todos los elementos del atacante.

O sea que si luchan Jack y Aku siendo Jack el primer atacante, Jack se verá afectado por el poder defensivo de la concentración y 
Aku se verá afectado por el poder ofensivo de la katana mágica, y la lucha continuará con Aku (luego del ataque) como atacante y con 
Jack (luego de la defensa) como defensor.


Inferir el tipo de la siguiente función:
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))
-}

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
 |estaMuerto atacante = (defensor, atacante)
 |otherwise = luchar proximoAtacante proximoDefensor
 where proximoAtacante = usarElementos ataque defensor (elementos atacante)
       proximoDefensor = usarElementos defensa atacante (elementos atacante)

-- Abstraemos cómo hacer para usar uno de los efectos de un conjunto de elementos sobre un personaje
usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)


afectar personaje funcion = funcion personaje


-- Punto 5 (inferencia)
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

f :: (Eq t1, Num t2) =>
     (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
