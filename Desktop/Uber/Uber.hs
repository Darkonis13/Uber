import Text.Show.Functions

data Chofer = Chofer {nombre:: String, km::Int, viajes :: [Viaje], condicion :: (Viaje->Bool)} deriving (Show)

data Viaje = Viaje {fecha :: (Int,Int,Int), precio::Int, cliente:: Cliente} deriving (Show)

data Cliente = Cliente {nombreC :: String, direccion :: String} deriving (Show)

type CondicionDeViaje =Viaje->Bool

jose = Chofer "Jose" 25000 [mdq] (condicion3 5)

abril = Cliente "Abril" "Floresta"

mdq = Viaje (3,7,18) 2500 abril

condicion1 :: CondicionDeViaje
condicion1 _ = True

condicion2 :: CondicionDeViaje
condicion2  = (>200).precio 

condicion3 ::  Int -> CondicionDeViaje
condicion3 letras =  (>letras).length.nombreC.cliente

condicion4 ::  String -> CondicionDeViaje
condicion4 zona =  not.(==zona).direccion.cliente


lucas = Cliente "Lucas" "Victoria"
daniel = Chofer "Daniel" 23500 [viajeLucas] (condicion4 "Olivos")
viajeLucas = Viaje (20,4,2017) 150 lucas 
alejandra = Chofer "Alejandra" 180000 [] condicion1

puedeTomarViaje :: Chofer->CondicionDeViaje
puedeTomarViaje   unChofer unViaje = (condicion unChofer) unViaje


liquidacion :: Chofer -> Int
liquidacion unChofer
 |(length.viajes) unChofer ==0 = 0
 |otherwise =(foldl1 (+).map precio.viajes) unChofer

listaChoferes :: [Chofer]
listaChoferes = [jose, daniel, alejandra]

filtrarChoferes :: Viaje-> [Chofer] -> [Chofer]
filtrarChoferes unViaje choferes = filter (flip puedeTomarViaje unViaje) choferes 

--Aca lo hicimos con menos KM en vez de menos cantidad de viajes
--choferMenosKm :: [Chofer] -> Chofer                  
--choferMenosKm choferes = foldl1 compararKm choferes

--compararKm ::  Chofer -> Chofer ->Chofer
--compararKm chofer1 chofer2
-- |(km chofer1)>(km chofer2) = chofer2
-- |otherwise = chofer1

choferMenosViajes :: [Chofer] -> Chofer
choferMenosViajes choferes = foldl1 compararViajes choferes


compararViajes ::  Chofer -> Chofer -> Chofer
compararViajes chofer1 chofer2
 |length(viajes chofer1)>length(viajes chofer2) = chofer2
 |otherwise = chofer1

efectuarViaje:: Viaje -> Chofer -> Chofer
efectuarViaje unViaje unChofer = unChofer{viajes = viajes unChofer ++ [unViaje]}


realizarViaje :: Viaje->[Chofer]-> Chofer
realizarViaje unViaje = efectuarViaje unViaje.choferMenosViajes.(filtrarChoferes unViaje)

nitoinfy = Chofer "nito infy xd" 70000 (repeat viajeConLucas) (condicion3 3)
viajeConLucas = Viaje (11,3,2017) 50 lucas
viajeDePrueba = Viaje (2,5,2017) 500 lucas


gongNeng :: Ord a => a->(a->Bool)->(c->a)->[c]->a 
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3
