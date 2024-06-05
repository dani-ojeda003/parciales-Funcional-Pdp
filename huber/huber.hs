import Text.Show.Functions()


data Chofer= UnChofer{
    nombre::String,
    km ::Int,
    viajes ::[Viaje],
    condicion :: Condicion
} deriving Show


data Cliente= UnCliente{
    nombreCliente:: String,
    dondeVive::String
} deriving Show

data Viaje = UnViaje{
    fecha::String,
    cliente:: Cliente,
    costo::Int
} deriving Show

type Condicion=Viaje->Bool

{-
algunos choferes toman cualquier viaje
otros solo toman los viajes que salgan más de $ 200
otros toman aquellos en los que el nombre del cliente tenga más de n letras
y por último algunos requieren que el cliente no viva en una zona determinada

-}

lucas :: Cliente
lucas = UnCliente "lucas" "Victoria"

daniel :: Chofer
daniel = UnChofer "daniel" 23500 [viajeLucas] (noVivanEn "Olivos")

alejandra:: Chofer
alejandra = UnChofer "alejandra" 180000 [] aceptaCualquierViaje 

nito ::Chofer
nito = UnChofer "Nito Infy" 70000 [infinitosViajesLucas..] (aceptaConMasDeNLetras 2)

infinitosViajesLucas::Viaje
infinitosViajesLucas= UnViaje "11/03/2017" lucas 50 

viajeLucas ::Viaje
viajeLucas = UnViaje "20/04/2017" lucas 150

{-
 Implementar con las abstracciones que crea conveniente las condiciones que cada chofer tiene para tomar un viaje. Debe utilizar en este punto composición y aplicación parcial.
( Definir las siguientes expresiones: 
el cliente “Lucas” que vive en Victoria
el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente Lucas el 20/04/2017 cuyo costo fue $ 150, y toma los viajes donde el cliente no viva en “Olivos”.
la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma cualquier viaje.
 Saber si un chofer puede tomar un viaje.
-}

aceptaCualquierViaje ::Condicion
aceptaCualquierViaje _ =True

salganMas :: Condicion
salganMas unViaje = costo unViaje > 200

aceptaConMasDeNLetras:: Int->Condicion
aceptaConMasDeNLetras cantLetras unViaje= cantidadLetrasDeUnNombre (cliente unViaje) > cantLetras

cantidadLetrasDeUnNombre :: Cliente->Int
cantidadLetrasDeUnNombre unCliente= length. nombreCliente$unCliente

noVivanEn:: String->Condicion
noVivanEn unaZona unViaje= (/= unaZona).dondeVive.cliente$ unViaje

aceptaViaje:: Viaje->Condicion->Bool
aceptaViaje  unViaje unaCondicion = unaCondicion unViaje

obtenerCostos:: Chofer->[Int]
obtenerCostos unChofer= map (costo) (viajes unChofer)

liquidacion:: Chofer->Int
liquidacion unChofer= sum.obtenerCostos$unChofer

realizarViajes:: Viaje->[Chofer]->[Chofer]
realizarViajes unViaje _ = []
realizarViajes unViaje (chofer1:chofer2:cola)= condicion (chofer1) || condicion (chofer2) && realizarViajes unViaje (chofer2:cola)


{-
Saber la liquidación de un chofer, que consiste en sumar los costos de cada uno de los viajes. Por ejemplo, Alejandra tiene $ 0 y Daniel tiene $ 150.
 Realizar un viaje: dado un viaje y una lista de choferes, se pide que
filtre los choferes que toman ese viaje. Si ningún chofer está interesado, no se preocupen: el viaje no se puede realizar.
considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.
efectuar el viaje: esto debe incorporar el viaje a la lista de viajes del chofer. ¿Cómo logra representar este cambio de estado?

Modelar al chofer “Nito Infy”, su auto tiene 70.000 kms., que el 11/03/2017 hizo infinitos viajes de $ 50 con Lucas y toma cualquier viaje donde el cliente tenga al menos 3 letras.

-}

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje unViaje = 
    efectuarViaje unViaje.choferConMenosViajes.filter (puedeTomarViaje unViaje)

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:cola)
  | cantidadDeViajes chofer1 < cantidadDeViajes chofer2 = choferConMenosViajes (chofer1:cola)
  | otherwise                                           = choferConMenosViajes (chofer2:cola)

cantidadDeViajes :: Chofer -> Int
cantidadDeViajes = length.viajesQueTomo

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje unViaje unChofer = unChofer { viajesQueTomo = unViaje : viajesQueTomo unChofer}


