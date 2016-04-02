module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

split :: Eq a => a -> [a] -> [[a]]
split d = foldr (\x r -> if (x == d) then []:r else (x:(head r)):(tail r)) [[]]


longitudPromedioPalabras :: Extractor
longitudPromedioPalabras t = mean ( map (genericLength) (split ' ' t) )


cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = [(elemCount x xs, x)| x <- nub(xs)]


elemCount :: Eq a => a -> [a] -> Int
elemCount a = foldr (\x r -> if (x==a) then r+1 else r ) 0 


repeticionesPromedio :: Extractor
repeticionesPromedio t = mean (map (fromIntegral . fst) (cuentas (split ' ' t) ) )

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [(\text -> freqRel text t) | t <- tokens]

freqRel :: Texto -> Char -> Feature
freqRel t c = fromIntegral(elemCount c t ) / (genericLength t)


--funcion falopa para probar normalizarExtractor como dice el enunciado
--uso: let txts = ["aa","ab","ugh"]
--  normalizarExtractor txts funcionMagica  "ugh"
funcionMagica :: Extractor
funcionMagica  = (\text -> if (text == "aa") then -20.3 else if (text == "ab") then 1.0 else 10.5 ) 			   

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor ts e = (\t -> (e t) / (foldr1 max ( map ((abs).(e)) ts) ) )

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures fs txts = [ (dameValores fs txts (txts!!i) ) | i <- [0..(length txts)-1] ]  --no me gusta esta resolucion revisar

dameValores :: [Extractor] -> [Texto] -> Texto -> Instancia
dameValores fs txts txt = [ (normalizarExtractor txts (fs!!i)) txt | i <- [0..length(fs)-1]]


distEuclideana :: Medida
distEuclideana p q = sqrt (sum (zipWith (\a b -> (a-b)**2) p q ) )

distCoseno :: Medida
distCoseno p q = ( sum (zipWith (\a b -> (a*b)) p q ))  / ( (normaVector p) * (normaVector q) )

normaVector :: [Feature] -> Float
normaVector = sqrt . (foldr (\x r -> x**2 + r) 0 ) -- tambien vale = (sqrt . sum . map (**2))


knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined
	
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
