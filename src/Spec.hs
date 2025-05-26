module Spec where
import PdePreludat
import Library
import Test.Hspec

-- Datos de prueba para los tests (pueden ir al inicio de tu Spec.hs)
-- Asegúrate de que auto3 esté definido en Library.hs y exportado.
-- (Asumo que lo está, basándome en el código que me pasaste antes)

baseAuto :: Auto
baseAuto = auto3 { desgasteChasis = 90, desgasteRuedas = 10, tiempoCarrera = 0 }

tramoBase :: Seccion
tramoBase = Curva { angulo = 60, longitud = 300 } -- La curva peligrosa

-- Función auxiliar para calcular el impacto puro de un tramo en un auto "limpio"
-- Necesaria para los tests de modMojado, modRipio y modTurbo
{-
impactoTramo :: Auto -> Seccion -> Auto
impactoTramo auto seccion =
    desgastePorTramo2 (auto {desgasteChasis = 0, desgasteRuedas = 0, tiempoCarrera = 0}) seccion
-}
-- Auto para probar el ripio en una recta (para que afecte el chasis)
baseAutoRipio :: Auto
baseAutoRipio = auto3 { desgasteChasis = 10, desgasteRuedas = 10, tiempoCarrera = 0 }
tramoRectaRipio :: Seccion
tramoRectaRipio = Recta { longitud = 1000 } -- Una recta que desgasta el chasis

correrTests :: IO ()
correrTests = hspec $ do

  describe "Test básico" $ do
    it "doble funciona correctamente" $
      doble 2 `shouldBe` 4

  ---
  describe "Modificadores de tramo" $ do

    -- modBoxes: Se repara y suma 10 segundos si no está en buen estado
    it "modBoxes repara el auto y suma 10 segundos si no esta en buen estado" $ do
      let autoDanado = baseAuto { desgasteChasis = 90, desgasteRuedas = 10 } -- Este auto NO esta en buen estado
      let sinMod = desgastePorTramo2 autoDanado tramoBase
      let conMod = aplicarTramoModificado autoDanado (TramoModificado tramoBase [modBoxes])
      
      -- El desgaste de ruedas debe ser 0 (reparado)
      desgasteRuedas conMod `shouldBe` 0
      -- El desgaste de chasis debe ser 15% (reparado)
      desgasteChasis conMod `shouldBe` 13.5 -- 90 * 0.15 = 13.5

      -- El tiempo de carrera debe ser el tiempo base del tramo + 10 segundos de boxes
      let tiempoBaseTramo = tiempoCarrera sinMod
      tiempoCarrera conMod `shouldBe` (tiempoBaseTramo + 10)

    it "modBoxes no hace nada si el auto esta en buen estado" $ do
      let autoSano = auto3 { desgasteChasis = 10, desgasteRuedas = 10, tiempoCarrera = 0 } -- Este auto SI esta en buen estado
      let sinMod = desgastePorTramo2 autoSano tramoBase
      let conMod = aplicarTramoModificado autoSano (TramoModificado tramoBase [modBoxes])
      
      -- No debe haber cambios adicionales a los del tramo base
      conMod `shouldBe` sinMod

    ---
    -- modMojado: Suma 50% más del tiempo de carrera que produce originalmente el tramo
    it "modMojado suma 50% mas del tiempo original del tramo" $ do
      let sinMod = desgastePorTramo2 baseAuto tramoBase
      let conMod = aplicarTramoModificado baseAuto (TramoModificado tramoBase [modMojado])
      
      -- Calculamos el tiempo que el tramo base agrega
      let tiempoOriginalTramo = tiempoCarrera (impactoTramo baseAuto tramoBase)
      -- El tiempo esperado es el tiempo base del tramo + 50% extra del tiempo original del tramo
      tiempoCarrera conMod `shouldBe` (tiempoCarrera sinMod + tiempoOriginalTramo * 0.5)

    ---
    -- modRipio: Doble de efecto de un tramo normal equivalente, y doble tiempo
    -- Usaremos una recta para probar el chasis.
    it "modRipio duplica el efecto de desgaste y tiempo en una recta" $ do
      let autoInicial = baseAutoRipio -- desgChasis = 10, desgRuedas = 10
      let sinMod = desgastePorTramo2 autoInicial tramoRectaRipio -- desgChasis: 10 + 10 = 20, desgRuedas: 10
      let conMod = aplicarTramoModificado autoInicial (TramoModificado tramoRectaRipio [modRipio])
      
      -- Impacto normal de la recta en chasis y ruedas (en un auto "limpio")
      let impactoNormalRecta = impactoTramo autoInicial tramoRectaRipio

      -- Desgaste de chasis: original + (impacto normal * 2)
      desgasteChasis conMod `shouldBe` (desgasteChasis autoInicial + desgasteChasis impactoNormalRecta * 2)

      -- Desgaste de ruedas: original + (impacto normal * 2)
      desgasteRuedas conMod `shouldBe` (desgasteRuedas autoInicial + desgasteRuedas impactoNormalRecta * 2)

      -- Tiempo: original + (impacto normal * 2)
      tiempoCarrera conMod `shouldBe` (tiempoCarrera autoInicial + tiempoCarrera impactoNormalRecta * 2)

    ---
    -- modObstruccion: Suma 2 puntos de desgaste en ruedas por cada metro
    it "modObstruccion suma desgaste de ruedas adicional al del tramo" $ do
      let metrosObstruccion = 10
      let sinMod = desgastePorTramo2 baseAuto tramoBase -- Ruedas de sinMod: 10 (base) + 15 (curva) = 25
      let conMod = aplicarTramoModificado baseAuto (TramoModificado tramoBase [modObstruccion metrosObstruccion])
      
      -- El desgaste esperado es el de sinMod (ya con la curva) + (metrosObstruccion * 2)
      desgasteRuedas conMod `shouldBe` (desgasteRuedas sinMod + metrosObstruccion * 2) -- Espera: 25 + 20 = 45

    ---
    -- modTurbo: Doble de velocidad máxima durante el tramo, velocidad restaurada al final.
    it "modTurbo reduce el tiempo del tramo y restaura la velocidad" $ do
      let autoOriginal = baseAuto { velocidadMaxima = 44, tiempoCarrera = 0 }
      let sinMod = desgastePorTramo2 autoOriginal tramoBase -- Tiempo sin turbo: 300 / (44/2) = 13.63...
      let conMod = aplicarTramoModificado autoOriginal (TramoModificado tramoBase [modTurbo])
      
      -- Calculamos el tiempo que el tramo agregaría con el turbo
      let autoConDobleVelocidadParaCalculo = autoOriginal { velocidadMaxima = velocidadMaxima autoOriginal * 2, tiempoCarrera = 0 }
      let impactoTiempoTurbo = tiempoCarrera (desgastePorTramo2 autoConDobleVelocidadParaCalculo tramoBase)

      -- El tiempo de carrera con turbo debe ser el tiempo base del auto + el impacto del tiempo con turbo
      tiempoCarrera conMod `shouldBe` (tiempoCarrera autoOriginal + impactoTiempoTurbo)
      -- El tiempo de carrera con turbo debe ser menor que el tiempo normal del tramo
      tiempoCarrera conMod `shouldSatisfy` (< tiempoCarrera sinMod)

      -- La velocidad máxima debe ser restaurada a la original
      velocidadMaxima conMod `shouldBe` velocidadMaxima autoOriginal


---

  describe "pasarPorTramo2" $ do

    it "el auto atraviesa el tramo si esta en buen estado de salud" $ do
      let autoSano = auto3 { desgasteChasis = 10, desgasteRuedas = 10, tiempoCarrera = 0 } -- Está en buen estado
      let tramoSimple = TramoModificado tramoBase [] -- Un tramo sin modificadores para simplificar
      
      let autoDespuesDeTramoNormal = desgastePorTramo2 autoSano tramoBase
      -- ¡Aquí es donde cambiamos el orden!
      let autoDespuesDePasar = pasarPorTramo2 autoSano tramoSimple 

      -- El auto debería ser exactamente igual a si hubiera pasado por el tramo normalmente
      autoDespuesDePasar `shouldBe` autoDespuesDeTramoNormal

    it "el auto no atraviesa el tramo si NO esta en buen estado de salud" $ do
      let autoDanado = baseAuto { desgasteChasis = 90, desgasteRuedas = 80, tiempoCarrera = 0 } -- NO está en buen estado (ruedas > 60)
      let tramoSimple = TramoModificado tramoBase [] -- Un tramo sin modificadores
      
      -- ¡Aquí es donde cambiamos el orden!
      let autoDespuesDePasar = pasarPorTramo2 autoDanado tramoSimple

      -- El auto debería ser exactamente el mismo que antes de intentar pasar por el tramo
      autoDespuesDePasar `shouldBe` autoDanado




{-
---------------------------
  describe "peganLaVuelta" $ do
    -- Definición de la pista, asumiendo que ya la tienes en Library.hs
    -- Asegúrate de que esta pista esté diseñada para cumplir las condiciones de tiempo y desgaste.
    -- Por ejemplo, si tus reglas son:
    -- - Recta de 200m -> 2.0s de tiempo
    -- - Curva de 40m -> 0.4s de tiempo
    -- - Curva de 190m -> 1.9s de tiempo (esta es la que rompe al Peugeot)
    -- Y el desgaste de las curvas es > 20% para el Peugeot (79% + >20% = >100%).
    let pistaLaManzana = vueltaALaManzana -- Asume que 'vueltaALaManzana' está en Library.hs


    let autosIniciales = [ferrari, peugeot] -- Asumo 'ferrari' y 'peugeot' ya definidos (como en mi respuesta anterior)
    let autosResultantes = peganLaVuelta pistaLaManzana autosIniciales
    let ferrariFinal = head autosResultantes -- Asume que Ferrari siempre está primero
--    let peugeotFinal = last autosResultantes  -- Asume que Peugeot siempre está último

    it "Una Ferrari y un Peugeot con desgaste 79 de ruedas pegan la vuelta a una pista vueltaALaManzana" $ do
      -- Test para la Ferrari
      -- Si tu implementación de 'tiempoCarrera' devuelve Float, usa `~=` para una comparación de flotantes.
      -- O si tu tipo `Numero` es `Double`, 'shouldBe' está bien.
      tiempoCarrera ferrariFinal `shouldBe` 11.6

      -- Test para el Peugeot
--      tiempoCarrera peugeotFinal `shouldBe` 3.9
      -- Verifica que el Peugeot ya no esté en buen estado (su desgaste superó el umbral)
--      buenEstadoSalud peugeotFinal `shouldBe` False


-}

