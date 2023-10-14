{- Santiago Batto : Algoritmos y estructuras de datos: Proyecto2.hs -}
{---------- 1 ----------}

{-- a --}
data Carrera = Matematica | Fisica | Computacion | Astronomia

{-- b --}
titulo :: Carrera -> String
titulo x = case x of
         Matematica -> "Licenciatura en Matematica"
         Fisica -> "Licenciatura en Fisica"
         Computacion -> "Licenciatura en CS"
         Astronomia -> "Licenciatura en Astronomia"
{- 
Ejecucion:
ghci> titulo Fisica
"Licenciatura en Fisica"
ghci> titulo Computacion
"Licenciatura en CS"
-}

{- Aclaracion, por pattern matching podria hacerse tambien sin usar sintaxis case. Pero me parecia mas efectivo y legible con dicha sintaxis. Dejo un uso de pattern matching diferente:
titulo :: Carrera -> String
titulo Matematica =  "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en CS"
titulo Astronomia = "Licenciatura en Astronomia"-}

{-- c --}
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si
   deriving (Eq, Ord, Show, Bounded)
{- Aclaracion: El deriving con las typeclasses se lo aplico por el punto 2 -}

{-- d --}
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano x = case x of
                        Do -> 'C'
                        Re -> 'D'
                        Mi -> 'E'
                        Fa -> 'F'
                        Sol -> 'G'
                        La -> 'A'
                        Si -> 'B'
{- 
Ejecucion:
ghci> cifradoAmericano Fa
'F'
ghci> cifradoAmericano Sol
'G'
ghci> cifradoAmericano Do 
'C'
-}

{---------- 2 ----------}
{- Necesitamos para hacer las operaciones:
   Eq, Ord: para utilizar el <=
   Show: para mostrar el minimo elemento al utilizar `min`
   Bounded: para mostrar el minimo elemento que piden en el ejercicio 3.c

Ejecucion:
ghci> Do <= Re
True
ghci> Fa `min` Sol
Fa
-}


{---------- 3 ----------}

{-- a --}
{-Corregido en el punto 1.C-}
minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)
{- 
Ejecucion:
ghci> minimoElemento [4,2,3,1,5]
1
ghci> minimoElemento [2]
2
-}

{-- b --}
minimoElemento' :: (Ord a, Bounded a)=> [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)
{-
Ejecucion: minimoElemento' ([1,5,10]::[Int])
-}

{-- c --}
{-
Ejecucion:
ghci> minimoElemento' ([Fa, La, Sol, Re, Fa]::[NotaBasica])
Re
-}

{---------- 4 ----------}

{-- a --}
type Altura = Int
type NumCamiseta = Int

data Zona = Arco | Defensa | Mediocampo | Delantera
    deriving (Eq, Show)
{- Agrego el Eq nada mas que para utilizarlo con el filter de contar_futbolistas_filter, ya que en contar_futbolistas no lo utilizo. Es mas, hay una aclaracion importante debajo de esa funcion de como seria contar_futbolistas si la consigna admitiese usar el "==" -}
data TipoReves = DosManos | UnaMano
   deriving Show
data Modalidad = Carretera | Pista | Monte | BMX
   deriving Show
data PiernaHabil = Izquierda | Derecha
   deriving Show

type ManoHabil = PiernaHabil

data Deportista =   Ajedrecista 
                  | Ciclista Modalidad 
                  | Velocista Altura 
                  | Tenista TipoReves ManoHabil Altura 
                  | Futbolista Zona NumCamiseta PiernaHabil Altura 
   deriving Show

{-- b --}
{-
Deportista es un tipo de dato algebraico en el cual, Ciclista es una funcion que toma un constructor parametrico de tipo Modalidad y devuelve un valor de tipo Deportista. Como bien se puede apreciar en:
ghci> :t Ciclista
Ciclista :: Modalidad -> Deportista
-}

{-- c --}
contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas (x:xs) = case x of
                              (Velocista _) -> 1 + contar_velocistas xs 
                              _ -> contar_velocistas xs
{- 
Ejecucion:
ghci> contar_velocistas [Ajedrecista, Tenista DosManos Derecha 150, Velocista 180, Futbolista Arco 1 Derecha 195, Velocista 200]    
2
ghci> contar_velocistas [Ajedrecista, Tenista DosManos Derecha 150, Futbolista Arco 1 Derecha 195]                              
0
 -}
{-- d --}
contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] z = 0
contar_futbolistas (x:xs) Arco       = case x of
                                           Futbolista Arco _ _ _    -> 1 + contar_futbolistas xs Arco
                                           _ -> contar_futbolistas xs Arco
contar_futbolistas (x:xs) Defensa    = case x of
                                           Futbolista Defensa _ _ _ -> 1 + contar_futbolistas xs Defensa
                                           _ -> contar_futbolistas xs Defensa
contar_futbolistas (x:xs) Mediocampo = case x of
                                           Futbolista Mediocampo _ _ _ -> 1 + contar_futbolistas xs Mediocampo
                                           _ -> contar_futbolistas xs Mediocampo
contar_futbolistas (x:xs) Delantera  = case x of
                                           Futbolista Delantera _ _ _ -> 1 + contar_futbolistas xs Delantera
                                           _ -> contar_futbolistas xs Delantera
{- 
Ejecucion: 
ghci> contar_futbolistas [Ajedrecista, Tenista DosManos Derecha 150, Futbolista Arco 1 Derecha 195, Futbolista Defensa 3 Izquierda 184, Futbolista Delantera 9 Derecha 179] Arco
1
ghci> contar_futbolistas [Ajedrecista, Tenista DosManos Derecha 150, Futbolista Arco 1 Derecha 195, Futbolista Defensa 3 Izquierda 184, Futbolista Delantera 9 Derecha 179] Delantera
1
ghci> contar_futbolistas [Ajedrecista, Tenista DosManos Derecha 150, Futbolista Arco 1 Derecha 195, Futbolista Defensa 3 Izquierda 184, Futbolista Delantera 9 Derecha 179] Mediocampo
0
 -}

{- 
Aclaracion: Si pudiese usar el "==" para comparar, la funcion seria mucho mas sencilla. No tendria que hacer un case para cada zona y solo deberia usar pattern matching para la lista y el deportista de la lista. Al saltearme usar pattern matching para la zona (aunque cabe destacar que deberia agregar el deriving Eq en el constructor Zona), la funcion es mucho mas corta. Dejo aqui debajo el codigo de la funcion usando Eq en Zona y una ejecucion para probar que funciona. Dejo la funcion comentada ya que no la pide la consigna:
contar_futbolistas'' :: [Deportista] -> Zona -> Int
contar_futbolistas'' [] z = 0
contar_futbolistas'' ((Futbolista z _ _ _):xs) a | z == a = 1 + contar_futbolistas'' xs a
                                                 | otherwise = contar_futbolistas'' xs a
contar_futbolistas'' (_:xs) a = contar_futbolistas'' xs a
Ejecucion:
ghci> contar_futbolistas'' [Ajedrecista, Futbolista Arco 1 Derecha 195, Futbolista Defensa 3 Izquierda 184, Futbolista Delantera 9 Derecha 179, Futbolista Defensa 2 Derecha 183] Defensa
2
ghci> contar_futbolistas'' [Ajedrecista, Futbolista Arco 1 Derecha 195, Futbolista Defensa 3 Izquierda 184, Futbolista Delantera 9 Derecha 179, Futbolista Defensa 2 Derecha 183] Mediocampo
0
 -}

{-- e --}
esZonaCorrecta :: Zona -> Deportista -> Bool
esZonaCorrecta a (Futbolista z _ _ _) = z == a
esZonaCorrecta _ _ = False

contar_futbolistas_filter :: [Deportista] -> Zona -> Int
contar_futbolistas_filter [] a = 0
contar_futbolistas_filter xs a = length (filter (esZonaCorrecta a) xs )
{- Para el uso de filter, es necesario el uso del operador de comparacion "==", pues le debo pasar una funcion que compare dos zonas (siendo en este caso "a" la zona pasada como parametro a la funcion, y "z" la zona que es obtendia de cada futbolista proveido por la funcion filter) y devuelva un booleano -}
{- 
Ejecucion:
ghci> contar_futbolistas_filter [Ajedrecista, Tenista DosManos Derecha 150, Futbolista Arco 1 Derecha 195, Futbolista Defensa 3 Izquierda 184, Futbolista Delantera 9 Derecha 179] Arco
1
ghci> contar_futbolistas_filter [Ajedrecista, Tenista DosManos Derecha 150, Futbolista Arco 1 Derecha 195, Futbolista Defensa 3 Izquierda 184, Futbolista Delantera 9 Derecha 179] Mediocampo
0
 -}


{---------- 5 ----------}

{-- a --}
sonidoNatural :: NotaBasica -> Int
sonidoNatural x = case x of               
                     Do -> 0
                     Re -> 2
                     Mi -> 4
                     Fa -> 5
                     Sol -> 7
                     La -> 9
                     Si -> 11
{- 
Ejecucion:
ghci> sonidoNatural Mi
4
ghci> sonidoNatural Si
11
-}

{-- b --}
data Alteracion = Bemol | Natural | Sostenido

{-- c --}
data NotaMusical = Nota NotaBasica Alteracion

{-- d --}
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota a b) = case b of
                               Bemol -> sonidoNatural a - 1
                               Sostenido -> sonidoNatural a + 1
                               _ -> sonidoNatural a 
{- 
Ejecucion:
ghci> sonidoCromatico (Nota Re Bemol)
1
ghci> sonidoCromatico (Nota Sol Bemol) 
6
ghci> sonidoCromatico (Nota La Sostenido)
10
ghci> sonidoCromatico (Nota Do Sostenido)
1
ghci> sonidoCromatico (Nota Do Natural)
0
 -}

{-- e --}
instance Eq NotaMusical 
   where
         notaMusical1 == notaMusical2 = sonidoCromatico notaMusical1 == sonidoCromatico notaMusical2
{- Aclaracion uso de where: El uso con el operador de la forma que muestro debajo me parece mucho mas explicito de que estoy haciendo verdaderamente con la instanciacion. Aun asi, no es necesaria toda esa linea salvo por el where. Si es escrito de la siguiente forma sigue siendo totalmente valido:
instance Eq NotaMusical 
   where (==) :: NotaMusical -> NotaMusical -> Bool
         notaMusical1 == notaMusical2 = sonidoCromatico notaMusical1 == sonidoCromatico notaMusical2

A pesar de que me parezca personalmente mejor escrito de esta forma comentada, entiendo que no en todo entorno de ejecucion funciona de esta forma, y tiraria error a pesar de que a mi no me tire. Este código utiliza la extensión MultiParamTypeClasses para definir una instancia de la clase Eq, y en visual studio code se encuentra incluida. 
-}

{- 
Ejecucion:
ghci> (Nota Si Bemol) == (Nota La Sostenido)
True
ghci> (Nota Mi Sostenido) == (Nota Fa Natural)
True
ghci> (Nota Do Sostenido) == (Nota Sol Natural)
False
-}

{-- f --}
instance Ord NotaMusical
   where
         notaMusical1 <= notaMusical2 = sonidoCromatico notaMusical1 <= sonidoCromatico notaMusical2
{- 
Ejecucion:
ghci> (Nota Do Sostenido) <= (Nota Re Bemol)
True
ghci> (Nota Do Sostenido) <= (Nota La Bemol) 
True
ghci> (Nota Sol Sostenido) <= (Nota Re Natural)
False
-}


{---------- 6 ----------}

{-- a --}
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x
{- 
Ejecucion:
ghci> primerElemento []
Nothing
ghci> primerElemento [7,3]
Just 7
-}

{---------- 7 ----------}

data Cola = VaciaC | Encolada Deportista Cola
   deriving Show
   

{-- a --}
{- 1 -}
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada deportista cola) = Just cola
{- 
Ejecucion:
ghci> atender (Encolada Ajedrecista (Encolada Ajedrecista (Encolada Ajedrecista (Encolada (Futbolista Arco 99 Derecha 205) (Encolada Ajedrecista (VaciaC))))))
Just (Encolada Ajedrecista (Encolada Ajedrecista (Encolada (Futbolista Arco 99 Derecha 205) (Encolada Ajedrecista VaciaC))))
ghci> atender (Encolada (Velocista 800) (Encolada Ajedrecista (Encolada Ajedrecista (Encolada (Futbolista Arco 99 Derecha 205) (Encolada Ajedrecista (VaciaC))))))
Just (Encolada Ajedrecista (Encolada Ajedrecista (Encolada (Futbolista Arco 99 Derecha 205) (Encolada Ajedrecista VaciaC))))
ghci> atender VaciaC
Nothing
-}
{- 2 -}
encolar :: Deportista -> Cola -> Cola
encolar deportista VaciaC = Encolada deportista VaciaC
encolar deportista (Encolada d cola) = Encolada d (encolar deportista cola)
{- 
Ejecucion:
ghci> encolar (Ciclista BMX) (Encolada Ajedrecista (Encolada (Velocista 200) VaciaC))
Encolada Ajedrecista (Encolada (Velocista 200) (Encolada (Ciclista BMX) VaciaC))
ghci> encolar Ajedrecista VaciaC
Encolada Ajedrecista VaciaC
-}
{- 3 -}
busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC a = Nothing
busca (Encolada d cola) Arco = case d of
                                  Futbolista Arco _ _ _ -> Just d
                                  _ -> busca cola Arco
busca (Encolada d cola) Mediocampo = case d of
                                  Futbolista Mediocampo _ _ _ -> Just d
                                  _ -> busca cola Mediocampo
busca (Encolada d cola) Defensa = case d of
                                  Futbolista Defensa _ _ _ -> Just d
                                  _ -> busca cola Defensa
busca (Encolada d cola) Delantera = case d of
                                  Futbolista Delantera _ _ _ -> Just d
                                  _ -> busca cola Delantera

{-- b --}
{- Cola se parece al tipo lista. Pues VaciaC vendria a ser equivalente a "[]" y Encolada, equivalente a ":". -}

{---------- 8 ----------}
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)
   deriving Show
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

{-- a --}
type GuiaTelefonica = ListaAsoc String Int

{-- b --}
{- 1 -}
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b lista) = 1 + la_long lista
{- 
Ejecucion:
ghci> la_long (Nodo "Julian" 3232 (Nodo "Leonel" 1010 (Nodo "Lautaro" 4119 (Nodo "Angel" 1111 Vacia))))
4
ghci> la_long Vacia
0
-}
{- 2 -}
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia Vacia = Vacia
la_concat Vacia (Nodo a b lista) = Nodo a b lista
la_concat (Nodo a b lista) (Nodo c d lista2) = Nodo a b (la_concat lista (Nodo c d lista2))
{- 
Ejecucion:
ghci> la_concat (Nodo "Julian" 3232 (Nodo "Leonel" 1010 (Nodo "Lautaro" 4119 (Nodo "Angel" 1111 Vacia)))) (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia)))
Nodo "Julian" 3232 (Nodo "Leonel" 1010 (Nodo "Lautaro" 4119 (Nodo "Angel" 1111 (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))))))
-}
{- 3 -}
la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia a b = Vacia
la_agregar (Nodo a b lista) c d | a == c = Nodo c d lista
                                | otherwise = Nodo a b (la_agregar lista c d)
{- 
Ejecucion: 
ghci> la_agregar (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))) "Juan" 3333                                                                
Nodo "Juan" 3333 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))
ghci> la_agregar (Nodo "Julian" 3232 (Nodo "Leonel" 1010 (Nodo "Lautaro" 4119 (Nodo "Angel" 1111 Vacia)))) "Leonel" 5555                                                        
Nodo "Julian" 3232 (Nodo "Leonel" 5555 (Nodo "Lautaro" 4119 (Nodo "Angel" 1111 Vacia)))
-}
{- 4 -}
la_pares :: ListaAsoc a b -> [(a,b)]
la_pares Vacia = []
la_pares (Nodo a b lista) = (a,b) : la_pares lista
{- 
Ejecucion:
ghci> la_pares (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia)))
[("Juan",2343),("eafl;kjf",123423),("asdgf",12345)]
-}
{- 5 -}
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia a = Nothing
la_busca (Nodo a b lista) c | a == c = Just b
                            | otherwise = la_busca lista c
{- 
Ejecucion:
ghci> la_busca (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))) "asdgf"
Just 12345
ghci> la_busca (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))) "Rangalanga"
Nothing
ghci> la_busca (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))) "Juan"      
Just 2343
-}
{- 6 -}
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b 
la_borrar c Vacia = Vacia
la_borrar c (Nodo a b lista) | c == a = la_borrar c lista
                             | otherwise = Nodo a b (la_borrar c lista)
{- 
Ejecucion: 
ghci> la_borrar "asdgf" (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))) 
Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 Vacia)
ghci> la_borrar "a;lskdfj" (Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))) 
Nodo "Juan" 2343 (Nodo "eafl;kjf" 123423 (Nodo "asdgf" 12345 Vacia))
-}

{---------- 9 ----------}
{-- a --}
data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a )
   deriving Show

a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama arbol1 a arbol2) = 1 + a_long arbol1 + a_long arbol2
{- 
Ejecucion:
ghci> a_long (Rama (Rama (Hoja) 2 (Hoja)) 1 (Rama (Hoja) 2 (Hoja)))
3
-}

{-- b --}
a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama arbol1 a arbol2) = a_hojas arbol1 + a_hojas arbol2
{- 
Ejecucion: 
ghci> a_hojas (Rama (Rama (Hoja) 2 (Hoja)) 1 (Rama (Hoja) 2 (Hoja)))
4
-}

{-- c --}
a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama arbol1 a arbol2) = Rama (a_inc arbol1) (a+1) (a_inc arbol2)
{- 
Ejecucion:
ghci> a_inc(Rama (Rama (Hoja) 2 (Hoja)) 1 (Rama (Hoja) 2 (Hoja)))   
Rama (Rama Hoja 3 Hoja) 2 (Rama Hoja 3 Hoja)
-}

{-- d --}
a_map :: (a -> b) -> Arbol a -> Arbol b
a_map f Hoja = Hoja
a_map f (Rama arbol1 a arbol2) = Rama (a_map f arbol1) (f a) (a_map f arbol2)
{- 
Ejecucion:
ghci> a_map even (Rama (Rama (Hoja) 2 (Hoja)) 1 (Rama (Hoja) 2 (Hoja)))
Rama (Rama Hoja True Hoja) False (Rama Hoja True Hoja)
ghci> a_map signum (Rama (Rama (Hoja) 2 (Hoja)) 1 (Rama (Hoja) 2 (Hoja)))
Rama (Rama Hoja 1 Hoja) 1 (Rama Hoja 1 Hoja)
ghci> a_map abs (Rama (Rama (Hoja) 2 (Hoja)) 1 (Rama (Hoja) 2 (Hoja)))   
Rama (Rama Hoja 2 Hoja) 1 (Rama Hoja 2 Hoja)
-}

{-- a --}
data ABB a = VacioABB | RamaABB (ABB a) a (ABB a)
   deriving Show

{-- b --}
insertarABB :: Ord a => a -> ABB a -> ABB a
insertarABB c VacioABB = RamaABB VacioABB c VacioABB
insertarABB c (RamaABB arbolbin1 a arbolbin2) | c < a = RamaABB (insertarABB c arbolbin1) a arbolbin2
                                              | c > a = RamaABB arbolbin1 a (insertarABB c arbolbin2)
                                              | otherwise = RamaABB arbolbin1 a arbolbin2
{- 
Ejecucion:
ghci> insertarABB 4 (RamaABB (RamaABB (VacioABB) 2 (VacioABB)) 5 (RamaABB (VacioABB) 2 (VacioABB))) 
RamaABB (RamaABB VacioABB 2 (RamaABB VacioABB 4 VacioABB)) 5 (RamaABB VacioABB 2 VacioABB)
ghci> insertarABB 3 (RamaABB (RamaABB (VacioABB) 2 (VacioABB)) 7 (RamaABB (VacioABB) 2 (VacioABB))) 
RamaABB (RamaABB VacioABB 2 (RamaABB VacioABB 3 VacioABB)) 7 (RamaABB VacioABB 2 VacioABB)
ghci> insertarABB 7 (RamaABB (RamaABB (VacioABB) 2 (VacioABB)) 7 (RamaABB (VacioABB) 2 (VacioABB))) 
RamaABB (RamaABB VacioABB 2 VacioABB) 7 (RamaABB VacioABB 2 VacioABB)
-}

{-- c --}
buscarABB :: Eq a => a -> ABB a -> Bool
buscarABB c VacioABB = False
buscarABB c (RamaABB arbolbin1 a arbolbin2) | a == c = True
                                            | otherwise = buscarABB c arbolbin1 || buscarABB c arbolbin2
{- 
Ejecucion:
ghci> buscarABB 3 (RamaABB (RamaABB (VacioABB) 2 (VacioABB)) 7 (RamaABB (VacioABB) 2 (VacioABB)))   
False
ghci> buscarABB 7 (RamaABB (RamaABB (VacioABB) 2 (VacioABB)) 7 (RamaABB (VacioABB) 2 (VacioABB))) 
True
ghci> buscarABB 2 (RamaABB (RamaABB (VacioABB) 2 (VacioABB)) 7 (RamaABB (VacioABB) 2 (VacioABB))) 
True
ghci> buscarABB 0 (RamaABB (RamaABB (VacioABB) 2 (VacioABB)) 7 (RamaABB (VacioABB) 2 (VacioABB))) 
False
-}

{-- d --}
mayor_a_todos :: Ord a => a -> ABB a -> Bool
mayor_a_todos c VacioABB = True
mayor_a_todos c (RamaABB arbolbin1 a arbolbin2) | c > a = mayor_a_todos c arbolbin1 && mayor_a_todos c arbolbin2
                                                | otherwise = False

menor_a_todos :: Ord a => a -> ABB a -> Bool
menor_a_todos c VacioABB = True
menor_a_todos c (RamaABB arbolbin1 a arbolbin2) | c < a = menor_a_todos c arbolbin1 && menor_a_todos c arbolbin2
                                                | otherwise = False

verificarABB :: Ord a => ABB a -> Bool
verificarABB VacioABB = True
verificarABB (RamaABB arbolbin1 a arbolbin2) = mayor_a_todos a arbolbin1 && menor_a_todos a arbolbin2
{- 
Ejecucion:
ghci> verificarABB (RamaABB (RamaABB (VacioABB) 10 (VacioABB)) 2 (RamaABB (VacioABB) 11 (VacioABB)))
False
ghci> verificarABB (RamaABB (RamaABB (RamaABB (VacioABB) 1 (VacioABB)) 3 (RamaABB (VacioABB) 7 (VacioABB))) 5 (RamaABB (VacioABB) 8 (RamaABB (VacioABB) 10 (VacioABB))))
False

Cambie el 7 del segundo arbol por un 4 pues 4<5 para que de true
ghci> verificarABB (RamaABB (RamaABB (RamaABB (VacioABB) 1 (VacioABB)) 3 (RamaABB (VacioABB) 4 (VacioABB))) 5 (RamaABB (VacioABB) 8 (RamaABB (VacioABB) 10 (VacioABB))))
True
-}

segmentos :: [a] -> Int
segmentos [] = 1
segmentos (x:xs) = length xs + 1 + segmentos xs


buscat :: Int -> [Int] -> Int
buscat e [] = maxBound
buscat e (x:xs) | x == e = min 0 (buscat e xs)
                | otherwise = min maxBound (buscat e xs)

sumPot:: Int -> Int -> Int
sumPot x 0 = 1
sumPot x n = 1 + sumPot x (n-1) * x 


h :: Int -> Int -> Int
h c 1 = c + 1 
h c n = h c (n-1) + c

g :: Int -> Int
g 1 = 3
g n = 3 * (g (n-1) + h 2 (n-1))

alCubo :: Int -> Int
alCubo 1 = 1
alCubo n = alCubo (n-1) + g (n-1) + h 3 (n-1)

gn8 :: Int -> [Int] -> Int
gn8 n [] = 0
gn8 n (x:xs) = gn8 (n + x) xs

n8 :: [Int] -> Int
n8 xs = gn8 0 xs


gmf :: Int -> [Int] -> Int
gmf n [] = 0
gmf n (x:xs) | n == 8 = 1 + gmf (n + x) xs
             | otherwise = gmf (n + x) xs

mf :: [Int] -> Int
mf [] = 0
mf (x:xs) = gmf x xs

f :: [Int] -> Int
f [] = 0
f (x:xs) = gmf 0 (x:xs) + f xs
