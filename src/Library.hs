module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--------------------------------------------------------------------------------------------------------------------------------------

-- Defino los tipos de datos
type Marca = String
type Modelo = String
type Apodo = String
type Numero = Number
type DesgasteRuedas = Numero
type DesgasteChasis = Numero
type VelocidadMaxima = Numero
type TiempoCarrera = Numero

--Defino tipo de dato Auto
data Auto = Auto {
    marca :: Marca,
    modelo :: Modelo,
    desgasteRuedas :: DesgasteRuedas,
    desgasteChasis :: DesgasteChasis,
    velocidadMaxima :: VelocidadMaxima,
    tiempoCarrera :: TiempoCarrera,
    apodos :: [Apodo]
} deriving (Eq,Show)

--------------------------------------------------------------------------------------------------------------------------------------

--Defino algunos autos

auto1 :: Auto
auto1 = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 65,
    tiempoCarrera = 0,
    apodos = ["La nave", "El fierro", "Ferrucho"]
}

auto2 :: Auto
auto2 = Auto {
    marca = "Lamborghini",
    modelo = "Diablo",
    desgasteRuedas = 4,
    desgasteChasis = 7,
    velocidadMaxima = 73,
    tiempoCarrera = 0,
    apodos = ["Lambo", "La Bestia"]
}

auto3 :: Auto
auto3 = Auto {
    marca = "Fiat",
    modelo = "600",
    desgasteRuedas = 27,
    desgasteChasis = 33,
    velocidadMaxima = 44,
    tiempoCarrera = 0,
    apodos = ["La Bocha", "La bolita", "Fitito"]
}

auto4 :: Auto
auto4 = Auto {
    marca = "Peugeot",
    modelo = "504",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 40,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}



--agregado de autos para los test de pistas finales
ferrari :: Auto
ferrari = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 60,
    tiempoCarrera = 0,
    apodos = ["La nave", "El fierro", "Ferrucho"]
}

peugeot :: Auto
peugeot = Auto {
    marca = "Peugeot",
    modelo = "504",
    desgasteRuedas = 79,
    desgasteChasis = 0,
    velocidadMaxima = 40,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}
--

-- agrego autos en perfecto estado para test finales

competidor1 :: Auto
competidor1 = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 270,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}
competidor2 :: Auto
competidor2 = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 90,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}
competidor3 :: Auto
competidor3 = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 124,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}
competidor4 :: Auto
competidor4 = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 79,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}


--------------------------------------------------------------------------------------------------------------------------------------

--ESTADO DE SALUD DEL AUTO

--PUNTO A

-- Función para saber si un auto está en buen estado
{-
buenEstadoSalud :: Auto -> Bool
buenEstadoSalud auto
    | marca auto == "Peugeot" = False
    | (tiempoCarrera auto < 100) && (desgasteChasis auto < 20) = True
    | (desgasteChasis auto < 40) && (desgasteRuedas auto < 60) = False
    | otherwise = True
-}
buenEstadoSalud :: Auto -> Bool
buenEstadoSalud auto
    | marca auto == "Peugeot" = False
    | tiempoCarrera auto < 100 = desgasteChasis auto < 20
    | otherwise = desgasteChasis auto < 40 && desgasteRuedas auto < 60

estadoSalud :: Auto -> String
estadoSalud auto
    | buenEstadoSalud auto = "Está en buen estado"
    | otherwise = "No está en buen estado"

--PUNTO B

--Función que verifica si el primer apodo empieza con "La "
{-
primerApodoEmpiezaConLa :: Auto -> Bool
primerApodoEmpiezaConLa auto =
    case apodos auto of
        (primerApodo:_) -> take 3 primerApodo == "La "  -- Comparar con el primer apodo
        [] -> False  -- Si no hay apodos, devolver False
-}
primerApodoEmpiezaConLa :: Auto -> Bool
primerApodoEmpiezaConLa auto = not (null (apodos auto)) && take 3 (head (apodos auto)) == "La "


--Funcion para determinar si un auto no da mas
noDaParaMas :: Auto -> String
noDaParaMas auto
    | (desgasteChasis auto > 80 && primerApodoEmpiezaConLa auto) || (desgasteRuedas auto > 80 ) = "No da mas"
    | otherwise = "Da para mas"

--PUNTO C

--Funcion que cuenta cantidad de apodos
cantidadDeApodos :: Auto -> Numero
cantidadDeApodos auto = length(apodos auto)

esPar :: Auto -> Bool
esPar auto = even (cantidadDeApodos auto)

--Funcion para determinar si un auto es un chiche
{-
esChiche :: Auto -> String
esChiche auto
    | desgasteChasis auto < 20 && esPar auto = "Es un chiche"
    | desgasteRuedas auto < 50 && not (esPar auto) = "Es un chiche"
    | otherwise = "No es un chiche"
-}
esChiche :: Auto -> String
esChiche auto
    | desgasteChasis auto < 20 && esPar auto = "Es un chiche"
    | desgasteRuedas auto < 50 && (not . esPar) auto = "Es un chiche"
    | otherwise = "No es un chiche"


--PUNTO D

--Funcion para determinar si un auto es una joya
esUnaJoya :: Auto -> Bool
esUnaJoya auto = (desgasteChasis auto == 0) 
              && (desgasteRuedas auto == 0) 
              && (cantidadDeApodos auto <= 1)

esJoya :: Auto -> String
esJoya auto
    |  esUnaJoya auto = "Es Una Joya"
    |  otherwise = "No Es Una Joya"

--PUNTO E

--Funcion que cuenta caracteres del modelo
cantidadCaracteresModelo :: Auto -> Numero
cantidadCaracteresModelo auto = length (modelo auto)

--Funcion para determinar nivel de chetez
nivelDeChetez :: Auto -> Numero
nivelDeChetez auto = 20 * cantidadDeApodos auto * cantidadCaracteresModelo auto

--PUNTO F

--Funcion para determinar la capacidad supercalifragilisticaespialidosa de un auto
supercalifragilisticaespialidosa :: Auto -> Numero
supercalifragilisticaespialidosa = length . head . apodos

--PUNTO G

--Funcion para determinar el riesgo de un auto
riesgo :: Auto -> Numero
riesgo auto
    | buenEstadoSalud auto = velocidadMaxima auto * (1/10) * desgasteRuedas auto
    | not (buenEstadoSalud auto) = velocidadMaxima auto * (1/10) * desgasteRuedas auto * 2

--------------------------------------------------------------------------------------------------------------------------------------

--MANOS A LA OBRA

--Funcion reparar auto
repararAuto :: Auto -> Auto
repararAuto auto = auto {
    desgasteChasis = desgasteChasis auto * 0.15,
    desgasteRuedas = 0
}

--Funcion para aplicar penalidad
aplicarPenalidad :: Auto -> Numero -> Auto
aplicarPenalidad auto penalidad = auto{
    tiempoCarrera =  tiempoCarrera auto + penalidad
}

--Funcion para aplicar nitro a un auto
aplicarNitro :: Auto -> Auto
aplicarNitro auto = auto{
    velocidadMaxima = velocidadMaxima auto * 1.2
}

--Funcion para bautizar un auto
bautizar :: Auto -> Apodo -> Auto
bautizar auto apodoNuevo = auto { apodos = apodos auto ++ [apodoNuevo] }


--Funcion para cambiar marca y modelo de un auto
llevarDesarmadero :: Auto -> Marca -> Modelo -> Auto
llevarDesarmadero auto marcaNueva modeloNuevo = auto{
    marca = marcaNueva,
    modelo = modeloNuevo,
    apodos = ["Nunca Taxi"]
}

--------------------------------------------------------------------------------------------------------------------------------------

--Defino tipo de dato Seccion

type Nombre = String
type Pais = String
type PrecioBase = Numero
type Angulo = Numero
type Longitud = Numero
type CambiosDeDireccion = Numero
type Diametro = Numero

data Seccion = Curva {
    angulo :: Angulo,
    longitud :: Longitud
}| Recta {
    longitud :: Longitud
}| Zigzag {
    cambiosDeDireccion :: CambiosDeDireccion
}| Rulo {
    diametro :: Diametro
} deriving (Eq,Show)

--Defino algunas curvas

curvaPeligrosa :: Seccion
curvaPeligrosa = Curva{
    longitud = 300,
    angulo = 60
}

curvaTranca :: Seccion
curvaTranca = Curva{
    longitud = 550,
    angulo = 110
}

tramoRecto :: Seccion
tramoRecto = Recta{
    longitud = 715
}

tramito :: Seccion
tramito = Recta{
    longitud = 260
}

zigzagLoco :: Seccion
zigzagLoco = Zigzag{
    cambiosDeDireccion = 5
}

casiCurva :: Seccion
casiCurva = Zigzag{
    cambiosDeDireccion = 1
}

ruloClasico :: Seccion
ruloClasico = Rulo{
    diametro = 13
}

deseoDeMuerte :: Seccion
deseoDeMuerte = Rulo{
    diametro = 26
}

--Defino tipo de dato Pista

data Pista = Pista {
    nombre :: String,
    pais :: String,
    precio :: Numero,
    secciones :: [Seccion]
} deriving (Show, Eq)

--------------------------------------------------------------------------------------------------------------------------------------

--PISTAS

{-
--PUNTO A

desgastePorCurva :: Auto -> Seccion -> Auto
desgastePorCurva auto (Curva angulo longitud) = auto{
    desgasteRuedas = desgasteRuedas auto + 3 * longitud / angulo,
    tiempoCarrera = tiempoCarrera auto + longitud / (velocidadMaxima auto/2)
}
desgastePorCurva auto _ = auto -- no hago nada si no es una curva :)

--PUNTO B

desgastePorRecta :: Auto -> Seccion -> Auto
desgastePorRecta auto (Recta longitud) = auto{
    desgasteChasis = desgasteChasis auto + longitud * (1/100),
    tiempoCarrera = tiempoCarrera auto + longitud / velocidadMaxima auto
}
desgastePorRecta auto _ = auto -- si le lleva una curva no hace nada :)

--PUNTO C

desgastePorZigzag :: Auto -> Seccion -> Auto
desgastePorZigzag auto (Zigzag cambiosDeDireccion) = auto{
    desgasteChasis = desgasteChasis auto + 5, desgasteRuedas = desgasteRuedas auto + velocidadMaxima auto * cambiosDeDireccion / 10,
    tiempoCarrera = tiempoCarrera auto + cambiosDeDireccion * 3
}
desgastePorZigzag auto _ = auto -- para que solo funcione con zigzags :)

--PUNTO D

desgastePorRulo :: Auto -> Seccion -> Auto
desgastePorRulo auto (Rulo diametro) = auto {
    desgasteRuedas = desgasteRuedas auto + diametro * 1.5,
    tiempoCarrera = tiempoCarrera auto + 5 * diametro / velocidadMaxima auto
}
desgastePorRulo auto _ = auto -- para que solo funcione con rulos :)
-}

desgastePorTramo :: Auto -> Seccion -> Auto
desgastePorTramo auto (Curva angulo longitud) = auto {
    desgasteRuedas = desgasteRuedas auto + 3 * longitud / angulo,
    tiempoCarrera = tiempoCarrera auto + longitud / (velocidadMaxima auto / 2)
}
desgastePorTramo auto (Recta longitud) = auto {
    desgasteChasis = desgasteChasis auto + longitud * (1/100),
    tiempoCarrera = tiempoCarrera auto + longitud / velocidadMaxima auto
}
desgastePorTramo auto (Zigzag cambiosDeDireccion) = auto {
    desgasteChasis = desgasteChasis auto + 5, 
    desgasteRuedas = desgasteRuedas auto + velocidadMaxima auto * cambiosDeDireccion / 10,
    tiempoCarrera = tiempoCarrera auto + cambiosDeDireccion * 3
}
desgastePorTramo auto (Rulo diametro) = auto {
    desgasteRuedas = desgasteRuedas auto + diametro * 1.5,
    tiempoCarrera = tiempoCarrera auto + 5 * diametro / velocidadMaxima auto
}




--------------------------------------------------------------------------------------------------------------------------------------

-- OPCION DE RECURSION

--PUNTO A

--Funcion calcular nivel de joyez 
nivelDeJoyezAuto :: Auto -> Numero
nivelDeJoyezAuto auto
  | esUnaJoya auto && tiempoCarrera auto < 50 = 1
  | esUnaJoya auto = 2
  | otherwise = 0

nivelDeJoyezSerie :: [Auto] -> Numero
nivelDeJoyezSerie [] = 0 -- Caso base, si viene la lista vacía devuelvo 0
nivelDeJoyezSerie (auto:autos) = nivelDeJoyezAuto auto + nivelDeJoyezSerie autos

--PUNTO B

--Funcion grupo de autos para entendidos

paraEntendidos :: [Auto] -> Bool
paraEntendidos autos = not (null autos) && validar autos
{-
validar :: [Auto] -> Bool
validar [] = True
validar (auto:resto)
  | not (buenEstadoSalud auto) = False
  | tiempoCarrera auto > 200 = False
  | otherwise = validar resto
-}
validar :: [Auto] -> Bool
validar [] = True
validar (auto:resto)
  | buenEstadoSalud auto && tiempoCarrera auto <= 200 = validar resto
  | otherwise = False


---------------------------------------------------------------------------------- INICIO TP2 ----------------------------------------------------------------------------------

--diccionario de datos escuderias
type Presupuesto = Numero 
type CostoProporcionalAutos = Numero
costoBaseAutos :: CostoProporcionalAutos
costoBaseAutos = 1500

type CostoReparacionChasis = Numero
costoReparacionChasis :: CostoReparacionChasis
costoReparacionChasis = 500

type CostoNitro = Numero
costoBaseNitro :: CostoNitro
costoBaseNitro = 100

type AumentoXNitro = Numero
aumentoXNitro :: AumentoXNitro
aumentoXNitro = 1.2

type CostoConversionAFerrari = Numero
costoConversionAFerrari :: CostoConversionAFerrari
costoConversionAFerrari = 3500

-- fin diccionario de datos escuderias


-- Defino escuderias
data Escuderia = Escuderia {
    nombreEscuderia :: Nombre,
    autos :: [Auto],
    presupuesto :: Presupuesto
} deriving Show



escuderiaUtn :: Escuderia
escuderiaUtn = Escuderia {
    nombreEscuderia = "Escuderia UTN",
    autos = [auto1, auto2, auto3], -- autos definidos previamente
    presupuesto = 50000 -- presupuesto inicial
}



-- PUNTO A
agregarAutoAEscuderia :: Escuderia -> Auto -> Escuderia
agregarAutoAEscuderia escuderia auto 
    |verificarPresupuesto escuderia auto = escuderia {
        autos = autos escuderia ++ [auto],
        presupuesto = presupuesto escuderia - costoInscripcionAuto auto
    }
    | otherwise = escuderia

verificarPresupuesto :: Escuderia -> Auto -> Bool
verificarPresupuesto escuderia auto = presupuesto escuderia >= costoInscripcionAuto auto 

costoInscripcionAuto :: Auto -> Numero
costoInscripcionAuto auto = velocidadMaxima auto * costoBaseAutos



{- 
QUERIDA PROFESORA... ignora todo este choclo que hice, era antes de simplificar y unificar codigo, atte su no tan querido alumno: lucas

-- PUNTO B

-- NO TENES IDEA LO QUE ME COSTO PROFE... atte: lucas :)

-- Función principal que repara todos los autos de la escudería
repararEscuderia :: Escuderia -> Escuderia
repararEscuderia escuderia =
    Escuderia (nombreEscuderia escuderia)
              (autosReparados (autos escuderia) (presupuesto escuderia))
              (presupuestoFinalReparacion (autos escuderia) (presupuesto escuderia))

-- Obtiene la lista de autos reparados si hay presupuesto
autosReparados :: [Auto] -> Presupuesto -> [Auto]
autosReparados [] _ = []
autosReparados (auto:resto) presupuesto
    | verificoPresupuestoArreglo auto presupuesto =
        repararAuto auto : autosReparados resto (presupuesto - calcularCostoReparacion auto)
    | otherwise = auto : autosReparados resto presupuesto

-- Calcula el presupuesto final después de reparar los autos que se pudieron
presupuestoFinalReparacion :: [Auto] -> Presupuesto -> Presupuesto
presupuestoFinalReparacion [] presupuesto = presupuesto
presupuestoFinalReparacion (auto:resto) presupuesto
    | verificoPresupuestoArreglo auto presupuesto =
        presupuestoFinalReparacion resto (presupuesto - calcularCostoReparacion auto)
    | otherwise = presupuestoFinalReparacion resto presupuesto

calcularCostoReparacion :: Auto -> Presupuesto
calcularCostoReparacion auto = (desgasteChasis auto - desgasteChasis (repararAuto auto)) * costoReparacionChasis

verificoPresupuestoArreglo :: Auto -> Presupuesto -> Bool
verificoPresupuestoArreglo auto presupuesto = calcularCostoReparacion auto <= presupuesto -- esto es lo que se repararia del auto


-- PUNTO C



-- antes que digas algo... si, podria haber hecho una funcion calcularCosto donde mandara otra funcion que haga el calculo del tipo de costo y tambien el costoBase pero lo pense despues
-- asi que porfavor no me hagas re hacerlo 🙄
-- se que estoy repitiendo la logica de aca en adelante pero bueno... cosas que pasan, no hay tiempo para corregirlo por las otras materias 😥
calcularCostoNitro :: Auto -> Presupuesto
calcularCostoNitro auto = velocidadMaxima auto * costoBaseNitro

verificoPresupuestoNitro :: Auto -> Presupuesto -> Bool
verificoPresupuestoNitro auto presupuesto = calcularCostoNitro auto <= presupuesto

colocarNitroEscuderia :: Escuderia -> Escuderia
colocarNitroEscuderia escuderia =
    Escuderia (nombreEscuderia escuderia)
              (autosConNitro (autos escuderia) (presupuesto escuderia))
              (presupuestoFinalNitro (autos escuderia) (presupuesto escuderia))


autosConNitro :: [Auto] -> Presupuesto -> [Auto]
autosConNitro [] _ = []
autosConNitro (auto:resto) presupuesto
    | verificoPresupuestoNitro auto presupuesto =
        colocarNitro auto : autosConNitro resto (presupuesto - calcularCostoNitro auto)
    | otherwise = auto : resto

presupuestoFinalNitro :: [Auto] -> Presupuesto -> Presupuesto
presupuestoFinalNitro [] presupuesto = presupuesto
presupuestoFinalNitro (auto:resto) presupuesto
    | verificoPresupuestoNitro auto presupuesto =
        presupuestoFinalNitro resto (presupuesto - calcularCostoNitro auto)
    | otherwise = presupuesto

colocarNitro :: Auto -> Auto
colocarNitro auto = auto {
    velocidadMaxima = velocidadMaxima auto * aumentoXNitro
}

-}


-- creo una funcion generica que transforma la escuderia
{-                                                                                          PREFIERO ESTA SOLUCION DE ACA CON EL WHERE PERO SE QUE NO TE GUSTA QUE USEMOS
                                                                                            COSAS QUE NO VIMOS EN CLASES :(
aplicarTransformacionAEscuderia :: 
    (Auto -> Auto) -> 
    (Auto -> Presupuesto) -> 
    (Auto -> Presupuesto -> Bool) -> 
    Bool -> 
    Escuderia -> Escuderia

aplicarTransformacionAEscuderia transformar calcularCosto verificarCorte cortarAlFallar escuderia =
    Escuderia (nombreEscuderia escuderia)
              (transformarAutos (autos escuderia) (presupuesto escuderia))
              (presupuestoFinal (autos escuderia) (presupuesto escuderia))
  where
    transformarAutos [] _ = []
    transformarAutos (auto:resto) presupuesto
        | verificarCorte auto presupuesto =
            transformar auto : transformarAutos resto (presupuesto - calcularCosto auto)
        | cortarAlFallar = auto : resto  -- corta acá, mantiene el resto tal cual
        | otherwise = auto : transformarAutos resto presupuesto

    presupuestoFinal [] presupuesto = presupuesto
    presupuestoFinal (auto:resto) presupuesto
        | verificarCorte auto presupuesto =
            presupuestoFinal resto (presupuesto - calcularCosto auto)
        | cortarAlFallar = presupuesto -- corta acá, no sigue descontando
        | otherwise = presupuestoFinal resto presupuesto
-}

aplicarTransformacionAEscuderia ::
    (Auto -> Auto) ->
    (Auto -> Presupuesto) ->
    (Auto -> Presupuesto -> Bool) ->
    Bool ->
    Escuderia ->
    Escuderia
aplicarTransformacionAEscuderia transformar calcularCosto verificarCorte cortarAlFallar escuderia =
    Escuderia (nombreEscuderia escuderia)
              (transformarAutos transformar calcularCosto verificarCorte cortarAlFallar (autos escuderia) (presupuesto escuderia))
              (presupuestoFinal calcularCosto verificarCorte cortarAlFallar (autos escuderia) (presupuesto escuderia))

transformarAutos ::
    (Auto -> Auto) ->
    (Auto -> Presupuesto) ->
    (Auto -> Presupuesto -> Bool) ->
    Bool ->
    [Auto] ->
    Presupuesto ->
    [Auto]
transformarAutos _ _ _ _ [] _ = []
transformarAutos transformar calcularCosto verificarCorte cortarAlFallar (auto:resto) presupuesto
    | verificarCorte auto presupuesto =
        transformar auto : transformarAutos transformar calcularCosto verificarCorte cortarAlFallar resto (presupuesto - calcularCosto auto)
    | cortarAlFallar = auto : resto
    | otherwise = auto : transformarAutos transformar calcularCosto verificarCorte cortarAlFallar resto presupuesto

presupuestoFinal ::
    (Auto -> Presupuesto) ->
    (Auto -> Presupuesto -> Bool) ->
    Bool ->
    [Auto] ->
    Presupuesto ->
    Presupuesto
presupuestoFinal _ _ _ [] presupuesto = presupuesto
presupuestoFinal calcularCosto verificarCorte cortarAlFallar (auto:resto) presupuesto
    | verificarCorte auto presupuesto =
        presupuestoFinal calcularCosto verificarCorte cortarAlFallar resto (presupuesto - calcularCosto auto)
    | cortarAlFallar = presupuesto
    | otherwise = presupuestoFinal calcularCosto verificarCorte cortarAlFallar resto presupuesto



-- PUNTO A



calcularCostoReparacion :: Auto -> Presupuesto
calcularCostoReparacion auto = (desgasteChasis auto - desgasteChasis (repararAuto auto)) * costoReparacionChasis

verificoPresupuestoArreglo :: Auto -> Presupuesto -> Bool
verificoPresupuestoArreglo auto presupuesto = calcularCostoReparacion auto <= presupuesto -- esto es lo que se repararia del auto

repararEscuderia :: Escuderia -> Escuderia
repararEscuderia = aplicarTransformacionAEscuderia repararAuto calcularCostoReparacion verificoPresupuestoArreglo False


-- PUNTO B
-- SI, soy conciente que tambien puedo generalizar lo de calcular costos... pero porfavor seamos justos, ya generalice terrible funcion. atte: lucas 😅
calcularCostoNitro :: Auto -> Presupuesto
calcularCostoNitro auto = velocidadMaxima auto * costoBaseNitro

verificoPresupuestoNitro :: Auto -> Presupuesto -> Bool
verificoPresupuestoNitro auto presupuesto = calcularCostoNitro auto <= presupuesto

colocarNitro :: Auto -> Auto
colocarNitro auto = auto {
    velocidadMaxima = velocidadMaxima auto * aumentoXNitro
}


colocarNitroEscuderia :: Escuderia -> Escuderia
colocarNitroEscuderia = aplicarTransformacionAEscuderia colocarNitro calcularCostoNitro verificoPresupuestoNitro True


-- PUNTO C



calcularCostoFerrarizar :: Auto -> Presupuesto
calcularCostoFerrarizar auto = costoConversionAFerrari -- si, se que no uso el auto pero mi generalizacion lo requeria lamentablemente

verificoPresupuestoAFerrari :: Auto -> Presupuesto -> Bool
verificoPresupuestoAFerrari auto presupuesto = calcularCostoFerrarizar auto <= presupuesto

convertirAFerrari :: Auto -> Auto 
convertirAFerrari auto = auto {
    marca = "Ferrari",
    modelo = "F50"
}

convertirEscuderiaAFerraris :: Escuderia -> Escuderia
convertirEscuderiaAFerraris = aplicarTransformacionAEscuderia convertirAFerrari calcularCostoFerrarizar verificoPresupuestoAFerrari False


-- PUNTO D
-- estoy seguro que estaras contenta de ver una funcion tan bien planteada del vamos 😎

calcularCostosEscuderia ::
    (Auto -> Presupuesto) ->
    Escuderia ->
    Presupuesto

calcularCostosEscuderia funcionCalculoCostos escuderia = foldl (\acc auto -> acc + funcionCalculoCostos auto) 0 (autos escuderia)


calcularCostoReparacionEscuderia :: Escuderia -> Presupuesto
calcularCostoReparacionEscuderia = calcularCostosEscuderia calcularCostoReparacion





-- 3

-- PUNTO A
type VelocidadInicial = Numero
velocidadInicial :: VelocidadInicial
velocidadInicial = 100

autosInfinia :: [Auto]
autosInfinia = map (\n -> Auto "Ferrari" "F50" 0 1 (velocidadInicial * n) 0 []) [1..]


infinia :: Escuderia
infinia = Escuderia "Infinia" autosInfinia 5000

-- PUNTO B
{-
Contestar qué sucede si:
Se realiza una reparación en equipo de ese equipo.

se reparan todos los autos para los que hay presupuesto, despues se corta la ejecucion
ya que haskell no general la lista completa de inicio por el lazy evaluation


Se optimizan los autos de ese equipo.
si a optimizar te referis a poner nitro no pasaria nada ya que el primer auto ya se sale del presupuesto


Se ferrarizan sus autos.
se ferrarizarian X autos hasta acabar el presupuesto tal como se contesto en la "1"

Se quiere conocer el costo total de reparación del equipo.
Este caso rompe todo ya que no hay cortes, asi que me colgas la pc... no pienso probarlo

Justificar conceptualmente en cada caso.
-}






-- 4 Modificadores de tramos


-- re hago las funciones de pasar por tramos para no alterar las originales del tp1 
{-
desgastePorTramo2 :: Auto -> Seccion -> Auto
desgastePorTramo2 auto (Curva angulo longitud) = auto {
    desgasteRuedas = desgasteRuedas auto + 3 * longitud / angulo,
    tiempoCarrera = tiempoCarrera auto + longitud / (velocidadMaxima auto / 2)
}
desgastePorTramo2 auto (Recta longitud) = auto {
    desgasteChasis = desgasteChasis auto + longitud * (1/100),
    tiempoCarrera = tiempoCarrera auto + longitud / velocidadMaxima auto
}
desgastePorTramo2 auto (Zigzag cambiosDeDireccion) = auto {
    desgasteChasis = desgasteChasis auto + 5, 
    desgasteRuedas = desgasteRuedas auto + velocidadMaxima auto * cambiosDeDireccion / 10,
    tiempoCarrera = tiempoCarrera auto + cambiosDeDireccion * 3
}
desgastePorTramo2 auto (Rulo diametro) = auto {
    desgasteRuedas = desgasteRuedas auto + diametro * 1.5,
    tiempoCarrera = tiempoCarrera auto + 5 * diametro / velocidadMaxima auto
}
-}
desgastePorTramo2 :: Auto -> Seccion -> Auto
desgastePorTramo2 auto (Curva angulo longitud) = auto {
    desgasteRuedas = desgasteRuedas auto + 3 * longitud / angulo,
    tiempoCarrera = tiempoCarrera auto + longitud / (velocidadMaxima auto / 2)
}
desgastePorTramo2 auto (Recta longitud) = auto {
    desgasteChasis = desgasteChasis auto + longitud * (1/100),
    tiempoCarrera = tiempoCarrera auto + longitud / velocidadMaxima auto
}
desgastePorTramo2 auto (Zigzag cambiosDeDireccion) = auto {
    desgasteChasis = desgasteChasis auto + 5, 
    desgasteRuedas = desgasteRuedas auto + velocidadMaxima auto * cambiosDeDireccion / 10,
    tiempoCarrera = tiempoCarrera auto + cambiosDeDireccion * 3
}
desgastePorTramo2 auto (Rulo diametro) = auto {
    desgasteRuedas = desgasteRuedas auto + diametro * 1.5,
    tiempoCarrera = tiempoCarrera auto + 5 * diametro / velocidadMaxima auto
}



-- PUNTO A

type Boxes = Bool

pasarPorTramo :: Auto -> Seccion -> Boxes -> Auto
pasarPorTramo auto seccion boxes
    | not (buenEstadoSalud auto) && boxes = paradaEnBoxes (repararAuto auto)
    | otherwise = desgastePorTramo2 auto seccion

paradaEnBoxes :: Auto -> Auto
paradaEnBoxes auto = auto {tiempoCarrera = tiempoCarrera auto + 10} 





-- PUNTO B


type Modificador = Auto -> Seccion -> Auto

data TramoModificado = TramoModificado Seccion [Modificador]
    deriving (Show, Eq)

aplicarTramoModificado :: Auto -> TramoModificado -> Auto
aplicarTramoModificado auto (TramoModificado seccion modificadores) =
    foldl (\a m -> m auto seccion) (desgastePorTramo2 auto seccion) modificadores
{-
modBoxes :: Modificador
modBoxes auto seccion
    | not (buenEstadoSalud auto) = (paradaEnBoxes . repararAuto) auto
    | otherwise = auto
-}

modBoxes :: Modificador
modBoxes auto seccion
    | not (buenEstadoSalud auto) = 
        auto{
            tiempoCarrera = tiempoCarrera auto + 10,
            desgasteChasis = desgasteChasis auto * 0.15,
            desgasteRuedas = 0
        }
    | otherwise = auto



modMojado :: Modificador
modMojado auto seccion = auto {
    tiempoCarrera = tiempoCarrera auto + (tiempoCarrera (desgastePorTramo2 auto seccion) - tiempoCarrera auto) * 0.5
}


modRipio :: Modificador
modRipio auto seccion = auto {
    desgasteChasis = desgasteChasis auto + (desgasteChasis (desgastePorTramo2 auto seccion) - desgasteChasis auto),
    desgasteRuedas = desgasteRuedas auto + (desgasteRuedas (desgastePorTramo2 auto seccion) - desgasteRuedas auto),
    tiempoCarrera = tiempoCarrera auto + (tiempoCarrera (desgastePorTramo2 auto seccion) - tiempoCarrera auto)
}



modObstruccion :: Numero -> Modificador
modObstruccion metros auto _ = auto {
    desgasteRuedas = desgasteRuedas auto + metros * 2
}


{-
modTurbo :: Modificador
modTurbo auto seccion =
    restaurarVelocidad (desgastePorTramo2 (duplicarVelocidad auto) seccion) auto

duplicarVelocidad :: Auto -> Auto
duplicarVelocidad auto = auto { velocidadMaxima = velocidadMaxima auto * 2 }

restaurarVelocidad :: Auto -> Auto -> Auto
restaurarVelocidad autoActualizado autoOriginal =
    autoActualizado { velocidadMaxima = velocidadMaxima autoOriginal }
-}



duplicarVelocidad :: Auto -> Auto
duplicarVelocidad auto = auto { velocidadMaxima = velocidadMaxima auto * 2 }

impactoTramo :: Auto -> Seccion -> Auto
impactoTramo auto seccion =
    desgastePorTramo2 (auto {desgasteChasis = 0, desgasteRuedas = 0, tiempoCarrera = 0}) seccion


modTurbo :: Modificador
modTurbo auto seccion =
    -- Calcula el tiempo que el tramo agregaría normalmente si el auto comenzara con tiempoCarrera 0.
    ( \tiempoAgregadoNormal ->
        -- Calcula el tiempo que el tramo agregaría con el turbo si el auto comenzara con tiempoCarrera 0 y velocidad duplicada.
        ( \tiempoAgregadoTurbo ->
            -- Aplica el cambio de tiempo: (tiempo actual del auto) - tiempoAgregadoNormal + tiempoAgregadoTurbo
            -- Y restaura la velocidad máxima del auto a la que tenía originalmente.
            auto {
                tiempoCarrera = (tiempoCarrera auto) - tiempoAgregadoNormal + tiempoAgregadoTurbo,
                velocidadMaxima = velocidadMaxima auto -- Asegura que la velocidad máxima sea la original al final del tramo
            }
        ) (tiempoCarrera (impactoTramo (duplicarVelocidad (auto { tiempoCarrera = 0, desgasteChasis = desgasteChasis auto, desgasteRuedas = desgasteRuedas auto })) seccion))
    ) (tiempoCarrera (impactoTramo (auto { tiempoCarrera = 0, desgasteChasis = desgasteChasis auto, desgasteRuedas = desgasteRuedas auto }) seccion))









-- 5
{-
Realizar la función que haga pasarPorTramo/2 que, dado un tramo y un auto, 
hace que el auto atraviese el tramo, siempre y cuando no pase que no da más 
al inicio del tramo. 
Si el escenario es que no da más, entonces el auto no recibe ningún efecto, 
ya que no pasa por el tramo.

-}



pasarPorTramo2 :: Auto -> TramoModificado -> Auto
pasarPorTramo2 auto tramo
    | buenEstadoSalud auto = aplicarTramoModificado auto tramo
    | otherwise = auto












-- 6

{-
Atravesando pistas 
Crear la vueltaALaManzana. Es una pista que se llama “La manzana”, 
en “Italia”, con un precio de $30 y con:

tramo recto de 130m, 
curva de 13m de 90°,
tramo recto de 130m, 
curva de 13m de 90°,
tramo recto de 130m, 
curva de 13m de 90°,
tramo recto de 130m, 
curva de 13m de 90°.

-}
data Pista2 = Pista2 {
    nombre2 :: String,
    pais2 :: String,
    precio2 :: Numero,
    secciones2 :: [TramoModificado]
} deriving (Show, Eq)



-- PUNTO A
vueltaALaManzana :: Pista2
vueltaALaManzana = Pista2{
    nombre2 = "La manzana",
    pais2 = "Italia",
    precio2 = 30,
    secciones2 = [
        TramoModificado (Recta 130) [], 
        TramoModificado (Curva 90 13) [], 
        TramoModificado (Recta 130) [], 
        TramoModificado (Curva 90 13) [], 
        TramoModificado (Recta 130) [], 
        TramoModificado (Curva 90 13) [], 
        TramoModificado (Recta 130) [], 
        TramoModificado (Curva 90 13) []
        
    ]
}



-- PUNTO B

{-
Crear la superPista, en “Argentina”, con precio de $300 y con los siguientes tramos:
-   tramoRectoClassic
-   curvaTranca
-   2 tramitos consecutivos, pero el segundo está mojado y el primero con turbo
-   Rulo en el aire de 10m
-   Curva con ángulo de 80º, longitud 400m; con obstrucción de 2m
-   Curva con ángulo de 115º, longitud 650m
-   Tramo recto de 970m
-   curvaPeligrosa
-   tramito con ripio
-   Boxes con un Tramo Recto de 800m
-   casiCurva con una obstrucción de 5m
-   Tramo zig zag de 2 cambios
-   deseoDeMuerte, mojado y de ripio (“¿Cómo que de ripio? Si es un rulo... ¡se caen las piedras!”... Sí, nosotros nos preguntamos lo mismo)
-   ruloClasico
-   zigZagLoco
-}

{-
data Seccion = Curva {
    angulo :: Angulo,
    longitud :: Longitud
}| Recta {
    longitud :: Longitud
}| Zigzag {
    cambiosDeDireccion :: CambiosDeDireccion
}| Rulo {
    diametro :: Diametro
} deriving (Eq,Show)
-}




tramoRectoClassic :: TramoModificado
tramoRectoClassic = TramoModificado (Recta 100) [modBoxes] 

curvaTrancaModificado :: TramoModificado
curvaTrancaModificado = TramoModificado curvaTranca []

superPista :: Pista2
superPista = Pista2{
    nombre2 = "La manzana",
    pais2 = "Argentina",
    precio2 = 300,
    secciones2 = [
        tramoRectoClassic, 
        curvaTrancaModificado,
        TramoModificado tramito [modTurbo],
        TramoModificado tramito [modMojado],
        TramoModificado (Rulo 10) [],
        TramoModificado (Curva 80 400) [modObstruccion 2],
        TramoModificado (Curva 115 400) [],
        TramoModificado (Recta 970) [],
        TramoModificado curvaPeligrosa [],
        TramoModificado tramito [modRipio],
        TramoModificado (Recta 800) [modBoxes],
        TramoModificado casiCurva [modObstruccion 5],
        TramoModificado (Zigzag 2) [],
        TramoModificado deseoDeMuerte [modMojado, modRipio],
        TramoModificado ruloClasico [],
        TramoModificado zigzagLoco []
    ]
}




-- PUNTO C
{-
Hacer la función peganLaVuelta/2 que dada una pista y una lista de autos, 
hace que todos los autos den la vuelta (es decir, que avancen por todos los tramos), 
teniendo en cuenta que un auto que no da más “deja de avanzar”.
-}

peganLaVuelta :: Pista2 -> [Auto] -> [Auto]
peganLaVuelta pista autos = map (flip correrPista (secciones2 pista)) autos

correrPista :: Auto -> [TramoModificado] -> Auto
correrPista auto tramos = foldl pasarPorTramo2 auto tramos






-- 7 ¡Y llegaron las carreras!!


-- PUNTO A

-- Modelar una carrera que está dada por una pista y un número de vueltas.

data Carrera = Carrera{
    pista :: Pista2,
    vueltas :: Numero
} deriving (Show, Eq)



-- PUNTO B
{-
Representar el tourBuenosAires, 
una carrera que se realiza en la superPista y tiene 20 vueltas.
-}

tourBuenosAires :: Carrera
tourBuenosAires = Carrera{
    pista = superPista,
    vueltas = 20
}



-- PUNTO C

{-
Hacer que un conjunto de autos juegue una carrera, 
obteniendo los resultados parciales de cada vuelta, 
y la eliminación de los autos que no dan más en cada vuelta.

La forma de modelar/representar la respuesta queda a criterio del grupo, 
pero debe poder responder lo siguiente:

-   El auto ganador luego de todas las vueltas de la carrera.
-   El tiempo total del segundo.
-   El tiempo parcial tras 2 vueltas del primer auto.
-   Cantidad de autos que terminaron la carrera.

Se puede usar esta función, que dada una función de valoración y una lista, 
retorna esa lista ordenada en forma ascendente según los resultados de 
aplicar esa función a cada elemento:

quickSortBy :: Ord b => (a -> b) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy valoracion (x:xs) = anteriores ++ [x] ++ posteriores    
    where
        anteriores  = quickSortBy valoracion $ filter ((< valoracion x).valoracion)  xs
        posteriores = quickSortBy valoracion $ filter ((>= valoracion x).valoracion) xs

Ejemplo de uso:
> quickSortBy abs [4, -2, 1]
[1, -2, 4]

-}

-- funcion que nos dejan usar 😃
quickSortBy :: Ord b => (a -> b) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy valoracion (x:xs) = anteriores ++ [x] ++ posteriores    
    where
        anteriores  = quickSortBy valoracion $ filter ((< valoracion x).valoracion)  xs
        posteriores = quickSortBy valoracion $ filter ((>= valoracion x).valoracion) xs



{-
data ResultadoVuelta = ResultadoVuelta {
    nroDeVuelta :: Numero,
    autosDespuesDeVuelta :: [Auto]
}

data ResultadoCarrera = ResultadoCarrera {
    resultadosVueltas :: [ResultadoVuelta]
}


--peganLaVuelta :: Pista2 -> [Auto] -> [Auto]

correrCarrera :: Pista2 -> Numero -> [Auto] -> [ResultadoCarrera]
correrCarrera _ 0 autos = autos -- caso base
correrCarrera pista vueltasFaltantes autos = correrCarrera pista (vueltasFaltantes - 1) (peganLaVuelta pista autos) 
-}



{-
-- desde aca


data ResultadoVuelta = ResultadoVuelta {
    nroDeVuelta :: Numero,
    autosRestantes :: [Auto],
    autosEliminados :: [Auto]
} deriving Show

data ResultadoCarrera = ResultadoCarrera {
    resultadosVueltas :: [ResultadoVuelta]
} deriving Show


{-
simularCarrera :: Carrera -> [Auto] -> ResultadoCarrera
simularCarrera carrera autos = ResultadoCarrera (simularVueltas (vueltas carrera) (pista carrera) autos 1)
-}
simularCarrera :: Carrera -> [Auto] -> ResultadoCarrera
simularCarrera carrera autos =
  ResultadoCarrera (simularVueltas (vueltas carrera) (pista carrera) autos 1)


{-
simularVueltas :: Numero -> Pista2 -> [Auto] -> Numero -> [ResultadoVuelta]
simularVueltas 0 _ autos _ = []
simularVueltas _ _ [] _ = []
simularVueltas n pista autos turno =
  construirResultadoVuelta pista autos turno :
  simularVueltas (n - 1) pista (filtrarAutosQueSiguen (darUnaVuelta pista autos)) (turno + 1)
-}
simularVueltas :: Numero -> Pista2 -> [Auto] -> Numero -> [ResultadoVuelta]
simularVueltas 0 _ _ _ = []
simularVueltas _ _ [] _ = []
simularVueltas n pista autos turno =
  consVuelta pista autos turno n

{-
consVuelta :: Pista2 -> [Auto] -> Numero -> Numero -> [ResultadoVuelta]
consVuelta pista autos turno vueltasRestantes =
  construirResultadoVuelta pista autos turno :
  simularVueltas (vueltasRestantes - 1) pista (filtrarAutosQueSiguen (darUnaVuelta pista autos)) (turno + 1)
-}

consVuelta :: Pista2 -> [Auto] -> Numero -> Numero -> [ResultadoVuelta]
consVuelta pista autos turno vueltasRestantes =
  resultadoDesdeVuelta pista autos turno :
  simularVueltas (vueltasRestantes - 1) pista (autosQueSiguenEnVuelta pista autos) (turno + 1)


resultadoDesdeVuelta pista autos turno =
  ResultadoVuelta turno (filtrarAutosQueSiguen (darUnaVuelta pista autos)) (filtrarAutosQueNoDanMas (darUnaVuelta pista autos))



autosQueSiguenEnVuelta :: Pista2 -> [Auto] -> [Auto]
autosQueSiguenEnVuelta pista autos = filtrarAutosQueSiguen (darUnaVuelta pista autos)


{-
construirResultadoVuelta :: Pista2 -> [Auto] -> Numero -> ResultadoVuelta
construirResultadoVuelta pista autos turno = ResultadoVuelta turno autosQueSiguen autosEliminados
  where
    autosTrasVuelta = darUnaVuelta pista autos
    autosQueSiguen = filtrarAutosQueSiguen autosTrasVuelta
    autosEliminados = filtrarAutosQueNoDanMas autosTrasVuelta
-}
{-
construirResultadoVuelta :: Pista2 -> [Auto] -> Numero -> ResultadoVuelta
construirResultadoVuelta pista autos turno =
  resultadoDesdeAutosYTurno (darUnaVuelta pista autos) turno
-}
construirResultadoVuelta :: Pista2 -> [Auto] -> Numero -> ResultadoVuelta
construirResultadoVuelta pista autos turno =
  resultadoDesdeAutosYTurno (darUnaVuelta pista autos) turno

{-
resultadoDesdeAutosYTurno :: [Auto] -> Numero -> ResultadoVuelta
resultadoDesdeAutosYTurno autos turno =
  ResultadoVuelta turno (filtrarAutosQueSiguen autos) (filtrarAutosQueNoDanMas autos)
-}
resultadoDesdeAutosYTurno :: [Auto] -> Numero -> ResultadoVuelta
resultadoDesdeAutosYTurno autos turno =
  ResultadoVuelta turno (filtrarAutosQueSiguen autos) (filtrarAutosQueNoDanMas autos)


darUnaVuelta :: Pista2 -> [Auto] -> [Auto]
darUnaVuelta pista = map (flip correrPista (secciones2 pista))



filtrarAutosQueSiguen :: [Auto] -> [Auto]
filtrarAutosQueSiguen = filter daParaSeguir

filtrarAutosQueNoDanMas :: [Auto] -> [Auto]
filtrarAutosQueNoDanMas = filter (not . daParaSeguir)

daParaSeguir :: Auto -> Bool
daParaSeguir auto = noDaParaMas auto /= "No da mas"


carrerita :: Carrera
carrerita = Carrera{
    pista = superPista,
    vueltas = 3
}

-- hasta aca
-}


-- simularCarrera carreraEjemplo [auto1, auto2, auto3]

















carrerita :: Carrera
carrerita = Carrera{
    pista = superPista2,
    vueltas = 2
}






type CantidadVueltasTotales = Number
type NumeroDeVuelta = Number
type Tiempo = Number
type Posicion = Number


-- Tipos para resultados
data ResultadoVuelta = ResultadoVuelta {
    nroDeVuelta :: NumeroDeVuelta,
    autosRestantes :: [Auto],
    autosEliminados :: [Auto]
} deriving Show

data ResultadoCarrera = ResultadoCarrera {
    resultadosVueltas :: [ResultadoVuelta]
} deriving Show

-- Simula una carrera completa
simularCarrera :: Carrera -> [Auto] -> ResultadoCarrera
simularCarrera carrera autos =
  ResultadoCarrera (simularVueltas (vueltas carrera) (pista carrera) autos 1)

-- Simula las vueltas restantes
simularVueltas :: CantidadVueltasTotales -> Pista2 -> [Auto] -> NumeroDeVuelta -> [ResultadoVuelta]
simularVueltas 0 _ _ _ = []
simularVueltas _ _ [] _ = []
simularVueltas vueltasRestantes pista autos turno =
  consVuelta pista autos turno vueltasRestantes

-- Construye el resultado de una vuelta y continúa
consVuelta :: Pista2 -> [Auto] -> NumeroDeVuelta -> CantidadVueltasTotales -> [ResultadoVuelta]
consVuelta pista autos turno vueltasRestantes =
  resultadoDesdeVuelta pista autos turno :
  simularVueltas (vueltasRestantes - 1) pista (autosQueSiguenEnVuelta pista autos) (turno + 1)

-- Aplica los efectos de una vuelta y construye el resultado
resultadoDesdeVuelta pista autos turno =
  ResultadoVuelta turno (filtrarAutosQueSiguen (darUnaVuelta pista autos)) (filtrarAutosQueNoDanMas (darUnaVuelta pista autos))

-- Calcula autos que siguen después de una vuelta
autosQueSiguenEnVuelta :: Pista2 -> [Auto] -> [Auto]
autosQueSiguenEnVuelta pista autos = filtrarAutosQueSiguen (darUnaVuelta pista autos)

-- Aplica una vuelta completa a todos los autos
darUnaVuelta :: Pista2 -> [Auto] -> [Auto]
darUnaVuelta pista = map (flip correrPista (secciones2 pista))

-- Filtros
filtrarAutosQueSiguen :: [Auto] -> [Auto]
filtrarAutosQueSiguen = filter daParaSeguir

filtrarAutosQueNoDanMas :: [Auto] -> [Auto]
filtrarAutosQueNoDanMas = filter (not . daParaSeguir)

daParaSeguir :: Auto -> Bool
daParaSeguir auto = buenEstadoSalud auto 








-----
--cosas a testaer

vueltaALaManzana2 :: Pista2
vueltaALaManzana2 = Pista2{
    nombre2 = "La manzana",
    pais2 = "Italia",
    precio2 = 30,
    secciones2 = [
        TramoModificado (Recta 130) [], 
        TramoModificado (Curva 90 13) [], 
        TramoModificado (Recta 130) [], 
        TramoModificado (Curva 90 13) [], 
        TramoModificado (Recta 130) [], 
        TramoModificado (Curva 90 13) [], 
        TramoModificado (Recta 130) [], 
        TramoModificado (Curva 90 13) []
        
    ]
}


competidor6 :: Auto
competidor6 = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 20,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}


competidor7 :: Auto
competidor7 = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 100,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}



competidor8 :: Auto
competidor8 = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 0,
    velocidadMaxima = 65,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}




-----





{-
peganLaVuelta vueltaALaManzana [competidor6, competidor7, competidor8]
[ Auto
    { marca = "Ferrari"
    , modelo = "F50"
    , desgaste_ruedas = 1.7333333333333333
    , desgaste_chasis = 5.2
    , velocidad_maxima = 270
    , unidad_velocidad = ""
    , tiempo_de_carrera = 2.3111111111111111
    , apodos = [ "El rey del desierto" ]
    }
, Auto
    { marca = "Ferrari"
    , modelo = "F50"
    , desgaste_ruedas = 1.7333333333333333
    , desgaste_chasis = 5.2
    , velocidad_maxima = 100
    , unidad_velocidad = ""
    , tiempo_de_carrera = 6.24
    , apodos = [ "El rey del desierto" ]
    }
, Auto
    { marca = "Ferrari"
    , modelo = "F50"
    , desgaste_ruedas = 1.7333333333333333
    , desgaste_chasis = 5.2
    , velocidad_maxima = 65
    , unidad_velocidad = ""
    , tiempo_de_carrera = 9.6
    , apodos = [ "El rey del desierto" ]
    }
]

-}





superPista2 :: Pista2
superPista2 = Pista2{
    nombre2 = "La manzana",
    pais2 = "Argentina",
    precio2 = 300,
    secciones2 = [
        tramoRectoClassic, 
        curvaTrancaModificado,
--        TramoModificado tramito [modTurbo],
--        TramoModificado tramito [modMojado],
        TramoModificado (Rulo 10) [modBoxes]
--        TramoModificado (Curva 80 400) [modObstruccion 2],
--        TramoModificado (Curva 115 400) [],
--        TramoModificado (Recta 970) [],
--        TramoModificado curvaPeligrosa [],
--        TramoModificado tramito [modRipio],
--        TramoModificado (Recta 800) [modBoxes]
--        TramoModificado casiCurva [modObstruccion 5],
--        TramoModificado (Zigzag 2) [modBoxes]
--        TramoModificado deseoDeMuerte [modMojado, modRipio],
--        TramoModificado ruloClasico [],
--        TramoModificado zigzagLoco [modBoxes]
    ]
}



superPista3 :: Pista2
superPista3 = Pista2{
    nombre2 = "La manzana",
    pais2 = "Argentina",
    precio2 = 300,
    secciones2 = [
        TramoModificado tramito [modTurbo],
        TramoModificado tramito [modMojado],
        TramoModificado (Rulo 10) [],
        TramoModificado (Curva 80 400) [modObstruccion 2],
        TramoModificado deseoDeMuerte [modMojado, modRipio],
        TramoModificado (Recta 800) [],
        TramoModificado deseoDeMuerte [modMojado, modRipio,modBoxes],
        TramoModificado (Recta 800) []
    ]
}





--(simularCarrera carrerita [competidor6, competidor7, competidor8])

-- mostrar resultados pedidos de la carrera
{-
El auto ganador luego de todas las vueltas de la carrera.
El tiempo total del segundo.
El tiempo parcial tras 2 vueltas del primer auto.
Cantidad de autos que terminaron la carrera.
-}






type Puesto = Numero
type Vuelta = Numero





-- El auto ganador luego de todas las vueltas de la carrera.

ultimaVueltaCarrera :: ResultadoCarrera -> ResultadoVuelta
ultimaVueltaCarrera = head . reverse . resultadosVueltas


autoEnPosicion :: Puesto -> ResultadoCarrera -> Auto
autoEnPosicion posicion resultadoCarrera =
    (quickSortBy tiempoCarrera (autosRestantes (ultimaVueltaCarrera resultadoCarrera))) !! (posicion - 1)

buscarGanador :: ResultadoCarrera -> Auto
buscarGanador resultadpCarrera = autoEnPosicion 1 resultadpCarrera


--buscarGanador (simularCarrera carrerita [competidor6, competidor7, competidor8])





-- El tiempo total del segundo.

tiempoTotalPuesto :: Puesto -> ResultadoCarrera -> TiempoCarrera
tiempoTotalPuesto puesto resultadoCarrera = tiempoCarrera  (autoEnPosicion puesto resultadoCarrera) 

-- tiempoTotalPuesto 2 (simularCarrera carrerita [competidor6, competidor7, competidor8])







--El tiempo parcial tras 2 vueltas del primer auto.

buscarVueltaEnCarrera :: Vuelta -> ResultadoCarrera -> ResultadoVuelta
buscarVueltaEnCarrera vuelta resultadoCarrera = resultadosVueltas resultadoCarrera !! (vuelta - 1) 

-- tiempoTotalPuesto 1 (buscarVueltaEnCarrera 2 (simularCarrera carrerita [competidor6, competidor7, competidor8]))







--Cantidad de autos que terminaron la carrera.

--length (autosRestantes (ultimaVueltaCarrera (simularCarrera carrerita [competidor6, competidor7, competidor8])))




-- FUNCION MOSTRAR RESULTADOS SOLICITADOS
--mostrarResultadosCarrera :: ResultadoCarrera
mostrarResultadosCarrera :: ResultadoCarrera -> String
mostrarResultadosCarrera resultadoCarrera =
    "--- Resultados de la Carrera ---\n" ++
    "Ganador: " ++ show (buscarGanador resultadoCarrera) ++ "\n" ++
    "Tiempo del Segundo: " ++ show (tiempoTotalPuesto 2 resultadoCarrera) ++ "\n" ++
    "Tiempo del Ganador en Vuelta 2: " ++ show (tiempoCarrera (head (autosRestantes (buscarVueltaEnCarrera 2 resultadoCarrera)))) ++ "\n" ++
    "Cantidad de autos que terminaron: " ++ show(length (autosRestantes (ultimaVueltaCarrera resultadoCarrera)))++ "\n" ++
    "--------------------------------"


-- mostrarResultadosCarrera (simularCarrera carrerita [competidor6, competidor7, competidor8])



-- PUNTO FINAL







