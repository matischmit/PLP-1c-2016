-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
 	"split" ~: testsSplit,
 	"longitudPromedioPalabras" ~: testsLongitudPromedioPalabras, 
 	"cuentas" ~: testsCuentas,
 	"repeticionesPromedio" ~: testRepeticionesPromedio,
 	"frecuenciaTokens" ~: testFrecuenciaTokens,
 	"normalizarExtractor" ~: testNormalizarExtractor,
 	"extraerFeatures" ~: testExtraerFeatures,
 	"knn" ~: testknn,
 	"accuracy" ~: testAccuracy
 	]

testsSplit = test [
 	split ',' ",PLP," ~?= ["PLP"],
 	split ',' " ,PLP, " ~?= [" ","PLP"," "]
  	]

testsLongitudPromedioPalabras = test [
	longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4,
	longitudPromedioPalabras "aa aa aa aa" ~?= 2
	]

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")]
	]

testRepeticionesPromedio = test [
	repeticionesPromedio "hola chau hola a hola hola" ~?= 2,
	repeticionesPromedio "a a a a a" ~?= 5
	]
	
testFrecuenciaTokens = test [
	(head frecuenciaTokens) "_a_b_c_d" ~?= 0.5
	]
	
testNormalizarExtractor = test [
	(normalizarExtractor ["abc abcd ab", "a", "abcd"] longitudPromedioPalabras) "abc abcd abcde" ~?= 1 -- normalizar divide por 4 en este caso
	]

testExtraerFeatures = test [
	extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["abc abc abcdefg abc", "a b c d"] ~?= [[1.0, 1.0],[0.25, 0.5]] -- frecuencias y repeticiones despues de normalizar
	]
	
testknn = test [
	knn 2 [ [1,2], [2,2], [10,10], [10,12] ] ["F", "F", "I", "I"] distEuclideana [1,1] ~?= "F"
	]

testAccuracy = test [
	accuracy ["I", "I", "F", "F", "F", "I"] ["F", "F", "I", "F", "F", "I"] ~?= 0.5,
	accuracy ["I", "I", "F", "F", "F", "I"] ["F", "F", "I", "I", "I", "F"] ~?= 0, 
	accuracy ["F", "F", "I", "F", "F", "I", "F"] ["F", "F", "I", "F", "F", "I", "F"] ~?= 1
	]
	
