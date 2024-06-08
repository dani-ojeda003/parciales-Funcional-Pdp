import Text.Show.Functions 


type Criatura = (Peligrosidad,Requisito)
type Peligrosidad = Int
type Requisito = Persona->Bool
{-
Sabemos que existen distintas criaturas, de las cuales nos interesa poder determinar cuál es su nivel de peligrosidad y qué 
tiene que cumplir una persona para deshacerse de ellas. En principio queremos contemplar las siguientes criaturas:

El siempredetras: la peligrosidad de esta criatura legendaria es 0, ya que no le hace nada a la persona que está acechando, es tan 
inofensivo que nunca nadie pudo afirmar que estaba siendo acechado. Sin embargo, no hay nada que se pueda hacer para que te deje 
en paz.

Los gnomos: individualmente son inofensivos, pero se especializan en atacar en grupo. La peligrosidad es 2 elevado a la cantidad 
de gnomos agrupados. Una persona puede deshacerse de un grupo de gnomos si tiene un soplador de hojas entre sus ítems.

Los fantasmas: se categorizan del 1 al 10 dependiendo de qué tan poderosos sean, y el nivel de peligrosidad es esa categoría 
multiplicada por 20. Cada fantasma tiene un asunto pendiente distinto, con lo cual se debe indicar para cada uno qué tiene que 
cumplir la persona para resolver su conflicto.
-}


siempreDeAtras :: Criatura
siempreDeAtras = (0 , nada )

gnomos :: Int->Criatura
gnomos unaCantidad = (2^unaCantidad , esPortadorDelItem "soplador de hojas")

fantasmas :: Int->Requisito->Criatura
fantasmas unaCategoria unRequisito = (20*unaCategoria , unRequisito)

{-
Modelar a las personas, de las cuales nos interesa la edad, cuáles son los ítems que tiene y la cantidad de experiencia que tiene; 
y a las criaturas teniendo en cuenta lo descrito anteriormente, y lo que queremos hacer en el punto siguiente.
-}


data Persona= UnaPersona{
    edad :: Int,
    items :: [String],
    experiencia :: Int
}

{-
Modelar a las personas, de las cuales nos interesa la edad, cuáles son los ítems que tiene y la cantidad de experiencia que tiene; 
y a las criaturas teniendo en cuenta lo descrito anteriormente, y lo que queremos hacer en el punto siguiente.

Hacer que una persona se enfrente a una criatura, que implica que si esa persona puede deshacerse de ella gane tanta experiencia 
como la peligrosidad de la criatura, o que se escape (que le suma en 1 la experiencia, porque de lo visto se aprende) en caso de 
que no pueda deshacerse de ella.
-}
nada :: Persona->Bool
nada _ = False


modificarExperiencia :: (Int->Int)->Persona->Persona
modificarExperiencia unaFuncion unaPersona = unaPersona {experiencia = unaFuncion . experiencia $ unaPersona }

esPortadorDelItem :: String->Persona->Bool
esPortadorDelItem unItem unaPersona= elem unItem (items unaPersona)

enfrentamiento :: Persona->Criatura->Persona
enfrentamiento unaPersona unaCriatura
    | puedeVencerALaCriatura unaPersona unaCriatura = modificarExperiencia (+ fst unaCriatura) unaPersona
    | otherwise = modificarExperiencia (+ 1) unaPersona

puedeVencerALaCriatura :: Persona->Criatura->Bool
puedeVencerALaCriatura unaPersona unaCriatura= snd unaCriatura unaPersona

{-
Determinar cuánta experiencia es capaz de ganar una persona luego de enfrentar sucesivamente a un grupo de criaturas.

Mostrar un ejemplo de consulta para el punto anterior incluyendo las siguientes criaturas: al siempredetras, a un grupo de 
10 gnomos, un fantasma categoría 3 que requiere que la persona tenga menos de 13 años y un disfraz de oveja entre sus ítems 
para que se vaya y un fantasma categoría 1 que requiere que la persona tenga más de 10 de experiencia.
-}

experienciaDespuesDeEnfretarCriaturas :: [Criatura]->Persona->Int
experienciaDespuesDeEnfretarCriaturas unasCriaturas  = experiencia . enfrentarCriaturas unasCriaturas


enfrentarCriaturas :: [Criatura]->Persona->Persona
enfrentarCriaturas unasCriaturas unaPersona= foldl enfrentamiento unaPersona unasCriaturas



grupoDeCriaturas :: Persona->[Criatura]
grupoDeCriaturas unaPersona= [siempreDeAtras,gnomos 10, fantasmas 3 tieneMenosDe13YUnDisfrazDeOveja , fantasmas 1 tieneMasDe10DeExperiencia ]


tieneMenosDe13YUnDisfrazDeOveja :: Persona -> Bool
tieneMenosDe13YUnDisfrazDeOveja unaPersona = ((<13) . edad $ unaPersona) && esPortadorDelItem "disfraz de oveja" unaPersona

tieneMasDe10DeExperiencia :: Persona->Bool
tieneMasDe10DeExperiencia = (>10). experiencia 


{-
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
que a partir de dos listas retorne una lista donde cada elemento:
- se corresponda con el elemento de la segunda lista, en caso de que el mismo no cumpla con la condición indicada
- en el caso contrario, debería usarse el resultado de aplicar la primer función con el par de elementos de dichas listas
Sólo debería avanzarse sobre los elementos de la primer lista cuando la condición se cumple. 
> zipWithIf (*) even [10..50] [1..7]
[1,20,3,44,5,72,7] ← porque [1, 2*10, 3, 4*11, 5, 6*12, 7]
-}
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
zipWithIf _ _ _ [] = []
zipWithIf funcion condicion (x1:xs1) (x2:xs2) 
    | condicion x2 = (funcion x1 x2):(zipWithIf funcion condicion (x1:xs1) xs2)
    | otherwise = x2:(zipWithIf funcion condicion (x1:xs1) xs2)

{-
Notamos que la mayoría de los códigos del diario están escritos en código César, que es una simple sustitución de todas las letras
 por otras que se encuentran a la misma distancia en el abecedario. Por ejemplo, si para encriptar un mensaje se sustituyó la a por 
 la x, la b por la y, la c por la z, la d por la a, la e por la b, etc.. Luego el texto "jrzel zrfaxal!" que fue encriptado de esa 
 forma se desencriptaría como "mucho cuidado!".

Hacer una función abecedarioDesde :: Char -> [Char] que retorne las letras del abecedario empezando por la letra indicada. O sea, 
abecedarioDesde 'y' debería retornar 'y':'z':['a' .. 'x'].

Hacer una función desencriptarLetra :: Char -> Char -> Char que a partir una letra clave (la que reemplazaría a la a) y la letra 
que queremos desencriptar, retorna la letra que se corresponde con esta última en el abecedario que empieza con la letra clave. 
Por ejemplo: desencriptarLetra 'x' 'b' retornaría 'e'.

Hint: se puede resolver este problema sin tener que hacer cuentas para calcular índices ;)

Definir la función cesar :: Char -> String -> String que recibe la letra clave y un texto encriptado y retorna todo el texto
 desencriptado, teniendo en cuenta que cualquier caracter del mensaje encriptado que no sea una letra (por ejemplo '!') 
 se mantiene igual. Usar zipWithIf para resolver este problema.

Realizar una consulta para obtener todas las posibles desencripciones (una por cada letra del abecedario) usando cesar para el 
texto "jrzel zrfaxal!".
-}



abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = [letra..'z'] ++ (take ((length ['a'..letra]) - 1))['a'..letra]

-- Defino la función 
obtenerLetra :: [Char] -> [Char] -> Char -> Char
obtenerLetra (x:xs) (y:ys) letra 
    | x == letra = y
    | otherwise = obtenerLetra xs ys letra

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave letraADesencriptar = obtenerLetra (abecedarioDesde letraClave) (abecedarioDesde 'a') letraADesencriptar

-- Defino la función 
esUnaLetra :: Char -> Bool
esUnaLetra letra = elem letra ['a'..'z']

cesar :: Char -> String -> String
cesar letraClave textoEncriptado = zipWithIf desencriptarLetra esUnaLetra (abecedarioDesde letraClave) textoEncriptado

