import Text.Show.Functions


type Guantelete= (Material,[Gema])
type Material=String
type Gema= Personaje->Personaje

data Personaje = UnPersonaje{
    edad::Int,
    energia::Int,
    habilidades::[String],
    nombre::String,
    planeta::String
} deriving Show

type Universo = [Personaje]

dani ::Personaje
dani = UnPersonaje 40 1000 ["coger"] "dani" "tierra"

punisher:: Personaje 
punisher = UnPersonaje 38 350 ["Disparar con de todo","golpear"] "The Punisher"  "Tierra" 

{-
Un guantelete está hecho de un material  y sabemos las gemas que posee. 
También se sabe de los personajes que tienen una edad, una energía, una serie de habilidades  su nombre y en qué planeta viven.
Los fabricantes determinaron que cuando un guantelete está completo -es decir, tiene las 6 gemas posibles- y su material es “uru”,
se tiene la posibilidad de chasquear un universo que contiene a todos sus habitantes y reducir a la mitad la cantidad de dichos 
personajes. 

Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.

Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.

-}

chasquido:: Guantelete->Universo->Universo
chasquido unGuantelete unUniverso
    | guanteleteCompleto unGuantelete = take (div (cantidadDePersonajesDelUniverso unUniverso) 2 ) unUniverso
    |otherwise= unUniverso

guanteleteCompleto::Guantelete->Bool
guanteleteCompleto unGuantelete= estaHechoDeUru unGuantelete && cantidadGemas unGuantelete==6

estaHechoDeUru :: Guantelete->Bool
estaHechoDeUru unGuantelete = fst unGuantelete == "uru"

cantidadGemas :: Guantelete->Int
cantidadGemas unGuantelete= length (snd unGuantelete)

cantidadDePersonajesDelUniverso :: Universo->Int
cantidadDePersonajesDelUniverso=length


universoAptoParaPendex :: Universo -> Bool 
universoAptoParaPendex = any $ (<=45).edad

energiaTotal :: Universo->Int
energiaTotal = sum . map energia . filter ((>1). length. habilidades) 

{-

La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.

El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. 
Además le quita 10 puntos de energía. 
El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.

El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).

El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, no puede dejar la edad del 
oponente con menos de 18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, 
le quedarán 22 (por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.

La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.

-}

modificarEnergia:: (Int->Int)->Personaje->Personaje
modificarEnergia unaFuncion unPersonaje= unPersonaje{energia= unaFuncion.energia $ unPersonaje }

modificarHabilidad:: ([String]->[String])->Personaje->Personaje
modificarHabilidad unaFuncion unPersonaje= unPersonaje{habilidades= unaFuncion.habilidades $ unPersonaje }

modificarPlaneta :: (String->String)->Personaje->Personaje
modificarPlaneta unaFuncion unPersonaje= unPersonaje {planeta= unaFuncion. planeta $ unPersonaje}

mente :: Int->Gema
mente energia = modificarEnergia (subtract energia)


alma :: String -> Gema
alma unaHabilidad unPersonaje = modificarEnergia (subtract 10) unPersonaje {
  habilidades = filter (/=unaHabilidad) $ habilidades unPersonaje 
  }

espacio :: String->Gema
espacio unPlaneta= modificarEnergia (subtract 20). modificarPlaneta (const unPlaneta) 

poder :: Gema
poder unPersonaje
  | cantidadDeHabilidades unPersonaje <= 2 = quitarHabilidades. modificarEnergia (const 0) $ unPersonaje
  | otherwise = modificarEnergia (const 0) unPersonaje

quitarHabilidades:: Gema
quitarHabilidades = modificarHabilidad (const [])


cantidadDeHabilidades :: Personaje->Int
cantidadDeHabilidades unPersonaje = length (habilidades unPersonaje)

modificarEdad :: (Int->Int)->Personaje->Personaje
modificarEdad unaFuncion unPersonaje= unPersonaje {edad= unaFuncion. edad $ unPersonaje }

{-
tiempo :: Gema
tiempo personaje = quitarEnergia 50 personaje {
  edad = (max 18.div (edad personaje)) 2 
}
-}

tiempo :: Gema
tiempo unPersonaje
  | div (edad unPersonaje) 2 >18 = modificarEdad (`div` 2 ). modificarEnergia (subtract 50) $ unPersonaje
  | otherwise = modificarEdad (const 18). modificarEnergia (subtract 50) $ unPersonaje


gemaLoca :: Gema->Gema
gemaLoca unaGema = unaGema. unaGema  

{-
Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” 
y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.
-}

guanteleteGoma :: Guantelete
guanteleteGoma = ("Goma",[tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")])

{-
No se puede utilizar recursividad. Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta el poder de 
cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.
-}

utilizar :: [Gema]->Personaje->Personaje
utilizar unasGemas unPersonaje= foldr ($) unPersonaje unasGemas 


{-
Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del 
infinito que produce la pérdida más grande de energía sobre la víctima.
-}
gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa unPersonaje unGuantelete = gemaMasPoderosaDe unPersonaje $ snd unGuantelete

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe unPersonaje (gema1:gema2:gemas) 
    | (energia . gema1 $ unPersonaje) < (energia . gema2 $ unPersonaje) = gemaMasPoderosaDe unPersonaje (gema1:gemas)
    | otherwise = gemaMasPoderosaDe unPersonaje (gema2:gemas)

{-
 Dada la función generadora de gemas y un guantelete de locos:
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
gemaMasPoderosa punisher guanteleteDeLocos
-}

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = ("vesconite" , infinitasGemas tiempo)


usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas = (utilizar . take 3. snd) 

-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher
