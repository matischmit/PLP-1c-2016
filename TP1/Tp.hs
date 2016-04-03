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
split d xs = filter (not . null) (foldr (\x r -> if (x == d) then []:r else (x:(head r)):(tail r)) [[]] xs)


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
normalizarExtractor ts e t = (\t -> (e t) / (foldr (max . abs . e) 0 ts) ) t
--normalizarExtractor ts e = e

extraerFeatures :: [Extractor] -> [Texto] -> Datos
--extraerFeatures fs txts = extraerFeatures2 [normalizarExtractor txts e | e <- fs] txts

--extraerFeatures2 :: [Extractor] -> [Texto] -> Datos
--extraerFeatures2 es txts = [[e t | e <- es ] | t <- txts ]

extraerFeatures fs txts = [[e t | e <- es ] | t <- txts ] 
	where es = [normalizarExtractor txts e | e <- fs]
	
--extraerFeatures :: [Extractor] -> [Texto] -> Datos
--extraerFeatures fs txts = [[e2 t | e2 <- [normalizarExtractor txts e | e <- fs] ] | t <- txts ] 
--extraerFeatures fs txts = [[e2 t | t <- txts] | e2 <- [normalizarExtractor txts e | e <- fs] ]
----extraerFeatures fs txts = [ (dameValores fs txts elem ) | elem <- txts ]  --no me gusta esta resolucion revisar

dameValores :: [Extractor] -> [Texto] -> Texto -> Instancia
dameValores fs txts txt = [ (normalizarExtractor txts elem) txt | elem <- fs]


distEuclideana :: Medida
distEuclideana p q = sqrt (sum (zipWith (\a b -> (a-b)**2) p q ) )

distCoseno :: Medida
distCoseno p q = ( sum (zipWith (\a b -> (a*b)) p q ))  / ( (normaVector p) * (normaVector q) )

normaVector :: [Feature] -> Float
normaVector = sqrt . (foldr (\x r -> x*x + r) 0 ) -- tambien vale = (sqrt . sum . map (**2))


knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k dss es m x = palabraMasRepetida (map snd (take k (ordenarVecinos dss es m x)))

ordenarVecinos :: Datos -> [Etiqueta] -> Medida -> Instancia -> [(Float, Etiqueta)] -- devuelve un a lista de tuplas (distancia, etiqueta) ordenadas por distancia
ordenarVecinos dss es m x = sort (zip (map (m x) dss) es)

palabraMasRepetida :: (Ord a) => [a] -> a
palabraMasRepetida = snd . (foldr1 max) . cuentas

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = mean ( zipWith (\ a b -> if a==b then 1 else 0) xs ys )

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos ds es n p = (tomarTrain ds train1 train2 val, tomarVal ds val train1, tomarTrain es train1 train2 val, tomarVal es val train1) 
 where l = tamanioParticion ds n
       train1 = l * (p-1)
       val = l
       train2 = (n - p) * l

tomarTrain :: [a] -> Int -> Int -> Int -> [a]
tomarTrain xs t1 t2 d = (take t1 xs) ++ (take t2 (drop (t1 + d) xs))


--tomarTrain xs t1 t2 d = [ xs!!i | i <- [0..(length xs)] , ( (i < t1) || ( (i > t1+d) && (i<t1+d+t2) ) ) ]
--tomarTrain xs t1 t2 d = foldr () () ()

tomarVal :: [a] -> Int -> Int -> [a]
tomarVal xs t d = take t (drop d xs)  

tamanioParticion :: Datos -> Int -> Int
tamanioParticion ds n = div (length ds) n

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n ds es = mean [validateAccuracy (separarDatos ds es n p ) | p <- [1..n]]

validateAccuracy :: (Datos, Datos, [Etiqueta], [Etiqueta]) -> Float -- toma una particion y calcula el accuracy de los datos de validacion contra los de training
validateAccuracy (td, vd, te, ve) = accuracy ve (validate td te vd)

validate :: Datos -> [Etiqueta] -> Datos -> [Etiqueta] -- valida cada elemento de la particion de validación contra las de training con knn
validate ts es vs = map (knn 15 ts es distEuclideana) vs
