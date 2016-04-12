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
 	"elemCount" ~: testsElemCount,
 	"repeticionesPromedio" ~: testRepeticionesPromedio,
 	"frecuenciaTokens" ~: testFrecuenciaTokens,
 	"normalizarExtractor" ~: testNormalizarExtractor,
 	"extraerFeatures" ~: testExtraerFeatures,
 	"knn" ~: testknn,
 	"accuracy" ~: testAccuracy,
 	"normas"  ~: testNormas,
 	"tomarTrain" ~: testsTomarTrain
 	]

testsSplit = test [
 	split ',' ",PLP," ~?= ["PLP"],
 	split ',' " ,PLP, " ~?= [" ","PLP"," "],
 	split ' '  " Hola Materia   PLP  " ~?= ["Hola","Materia","PLP"], 
 	split 10 [4,10,8,15,10,16,23,42,10,10] ~?= [[4],[8,15],[16,23,42]]
  	]

testsLongitudPromedioPalabras = test [
	longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4,
	longitudPromedioPalabras "aa aa aa aa" ~?= 2
	]

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
	cuentas "Lorem ipsum dolor" ~?= [(1,'L'),(3,'o'),(2,'r'),(1,'e'),(2,'m'),(2,' '),(1,'i'),(1,'p'),(1,'s'),(1,'u'),(1,'d'),(1,'l')]
	]

testsElemCount = test [
	elemCount "x" ["x","x","y","x","z", "z", "z", "x", "y", "y", "z"] ~?= 4,
	elemCount "y" ["x","x","y","x","z", "z", "z", "x", "y", "y", "z"] ~?= 3,
	elemCount "z" ["x","x","y","x","z", "z", "z", "x", "y", "y", "z"] ~?= 4,
	elemCount "a" ["x","x","y","x","z", "z", "z", "x", "y", "y", "z"] ~?= 0
	]

testsTomarTrain = test [
	tomarTrain [1, 2, 3] 1 1 1 ~?= [1, 3],
	tomarTrain [1, 1, 2, 2, 3,  3] 2 2 2 ~?= [1,  1,  3,  3],
	tomarTrain [1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3] 1 4 6 ~?= [1,  3,  3,  3,  3],
	tomarTrain [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2] 0 1 10 ~?= [2]
	]

testRepeticionesPromedio = test [
	repeticionesPromedio "hola chau hola a hola hola" ~?= 2,
	repeticionesPromedio "a a a a a" ~?= 5,
	repeticionesPromedio "a a b b a a" ~?= 3,
	repeticionesPromedio "a,b,c,d,e,f,g" ~?= 1
	]

	
testFrecuenciaTokens = test [
	(head frecuenciaTokens) "_a_b_c_d" ~?= 0.5,
	(frecuenciaTokens!!32 ) "aaa beee cee" ~?= 0.25,	--3 'a'es de 12
	(frecuenciaTokens!!31) "EscribirPalabrasSinEspacioEnElMedio" ~?= 0.0,		--espacios
	map (frecuenciaTokens!!36) ["Frecuencia de eees", "Fricuincia di i", "eeeeeeeeei"] ~?= [0.33333334,0.0,0.9]
	]

testNormas = test [
	distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464,
	distEuclideana [1.0,0,0,0] [0,0,0,0] ~?= 1.0,
	distEuclideana [1.0,1.0,1.0,1.0] [2.0,2.0,2.0,2.0] ~?= 2.0,
	distEuclideana [0.0, 0.0] [1.0, 1.0] ~?= 1.4142135, --sqrt 2
	distCoseno [0,3,4] [0,-3,-4] ~?= -1.0,
	distCoseno [1,1,1] [100,100,100] ~?= 1.0
	]

testNormalizarExtractor = test [
	(normalizarExtractor ["abc abcd ab", "a", "abcd"] longitudPromedioPalabras) "abc abcd abcde" ~?= 1 -- normalizar divide por 4 en este caso
	]

testExtraerFeatures = test [
	extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["abc abc abcdefg abc", "a b c d"] ~?= [[1.0, 1.0],[0.25, 0.5]], -- frecuencias y repeticiones despues de normalizar
	extraerFeatures [palabrasClaves ] ["namespace a++; c=10; return(a+b);", "foldNada = fold", "tryClassifier :: [Texto] -> [Etiqueta] -> Float"] ~?= [[0.875],[-1.0],[-0.125]]

	]
	
testknn = test [
	knn 2 [ [1,2], [2,2], [10,10], [10,12] ] ["F", "F", "I", "I"] distEuclideana [1,1] ~?= "F"
	]

testAccuracy = test [
	accuracy ["I", "I", "F", "F", "F", "I"] ["F", "F", "I", "F", "F", "I"] ~?= 0.5,
	accuracy ["I", "I", "F", "F", "F", "I"] ["F", "F", "I", "I", "I", "F"] ~?= 0, 
	accuracy ["F", "F", "I", "F", "F", "I", "F"] ["F", "F", "I", "F", "F", "I", "F"] ~?= 1
	]
	
