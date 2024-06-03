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


