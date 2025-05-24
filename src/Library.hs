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
} deriving Show

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
} deriving Show

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
} deriving Show

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

aplicarTramoModificado :: Auto -> TramoModificado -> Auto
aplicarTramoModificado auto (TramoModificado seccion modificadores) =
    foldl (\a m -> m a seccion) (desgastePorTramo2 auto seccion) modificadores


modBoxes :: Modificador
modBoxes auto seccion
    | not (buenEstadoSalud auto) = (paradaEnBoxes . repararAuto) auto
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


modTurbo :: Modificador
modTurbo auto seccion =
    restaurarVelocidad (desgastePorTramo2 (duplicarVelocidad auto) seccion) auto

duplicarVelocidad :: Auto -> Auto
duplicarVelocidad auto = auto { velocidadMaxima = velocidadMaxima auto * 2 }

restaurarVelocidad :: Auto -> Auto -> Auto
restaurarVelocidad autoActualizado autoOriginal =
    autoActualizado { velocidadMaxima = velocidadMaxima autoOriginal }






