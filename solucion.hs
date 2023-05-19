-- Completar con los datos del grupo
--
-- Nombre de Grupo: wololo
-- Integrante 1: Tomas Ezama, tomasezama@gmail.com, 475/23 
-- Integrante 2: Eduardo Baars, eduardobaars@id.uff.br, 1338/21
-- Integrante 3: Mauricio Romero Laino, mauricioromerolaino@gmail.com, 18/23
-- Integrante 4: Cian Andrés Bautista, andycia802@gmail.com, 937/21

module Solucion where

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

---------------------------------------- EJERCICIO 1 ----------------------------------------

-- Devuelve una lista con todos los usuarios de la red social dada.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres(usuarios red)

proyectarNombres :: [Usuario] -> [String]
-- Dada una lista de usuarios, devuelve una lista con los nombres de los usuarios sin repeticiones.
proyectarNombres us = eliminarRepetidos (proyectarNombresConRepetidos (us))

proyectarNombresConRepetidos :: [Usuario] -> [String]
-- Dada una lista de usuarios, devuelve una lista con los nombres de los usuarios, pueden aparecer repeticiones.
proyectarNombresConRepetidos [] = []
proyectarNombresConRepetidos (x:xs) = nombreDeUsuario x : proyectarNombresConRepetidos xs


---------------------------------------- EJERCICIO 2 ----------------------------------------

-- Dada una red y un usuario, devuelve una lista con todos los amigos del usuario.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = auxiliarAmigosDe u (relaciones red)

-- Dado un usuario y una lista de relaciones, devuelve una lista con todos los amigos del usuario.
auxiliarAmigosDe :: Usuario -> [Relacion] -> [Usuario]
auxiliarAmigosDe _ [] = []
auxiliarAmigosDe u ((x, y):xs) | u == x = y : auxiliarAmigosDe u xs 
                               | u == y = x : auxiliarAmigosDe u xs
                               | otherwise = auxiliarAmigosDe u xs


---------------------------------------- EJERCICIO 3 ----------------------------------------

-- Dada una red y un usuario, devuelve la cantidad de amigos del usuario.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = longitud (amigosDe red usuario)


---------------------------------------- EJERCICIO 4 ----------------------------------------

-- Devuelve al usuario con más amigos en la red social dada.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = auxiliarUsuarioConMasAmigos (red) (usuarios (red))

auxiliarUsuarioConMasAmigos :: RedSocial -> [Usuario] -> Usuario
auxiliarUsuarioConMasAmigos red [u] = u
auxiliarUsuarioConMasAmigos red (x:y:xs) | cantidadDeAmigos red x >= cantidadDeAmigos red y = auxiliarUsuarioConMasAmigos red (x:xs)
                                         | otherwise = auxiliarUsuarioConMasAmigos red (y:xs)


---------------------------------------- EJERCICIO 5 ----------------------------------------

-- Devuelve True si existe un usuario en la red social dada que tenga más de 1000000 amigos. En otro caso, devuelve False.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = cantidadDeAmigos (red) (usuarioConMasAmigos (red)) > 1000000

-- Función auxiliar para realizar el testing de estaRobertoCarlos. Utiliza 4 en vez de 1000000.
estaRobertoCarlosTesteable4 :: RedSocial -> Bool
estaRobertoCarlosTesteable4 red = cantidadDeAmigos (red) (usuarioConMasAmigos (red)) > 4


---------------------------------------- EJERCICIO 6 ----------------------------------------

-- Devuelve una lista con todas las publicaciones hechas por el usuario dado, en la red social dada.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = auxiliarPublicacionesDe (publicaciones (red)) (u)

auxiliarPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
auxiliarPublicacionesDe [] _ = []
--auxiliarPublicacionesDe (x:xs) u = if (usuarioDePublicacion (x)) == u then (x) : auxiliarPublicacionesDe xs u else auxiliarPublicacionesDe xs u
auxiliarPublicacionesDe (x:xs) u | usuarioDePublicacion x == u = x : auxiliarPublicacionesDe xs u
                                 | otherwise = auxiliarPublicacionesDe xs u


---------------------------------------- EJERCICIO 7 ----------------------------------------

-- Devuelve una lista con todas las publicaciones "likeadas" por el usuario dado, en la red social dada.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = auxiliarPublicacionesQueLeGustanA (publicaciones (red)) (u)

auxiliarPublicacionesQueLeGustanA :: [Publicacion] -> Usuario -> [Publicacion]
auxiliarPublicacionesQueLeGustanA [] _ = []
auxiliarPublicacionesQueLeGustanA (x:xs) u | pertenece (u) (likesDePublicacion (x)) = x : auxiliarPublicacionesQueLeGustanA xs u
                                           | otherwise = auxiliarPublicacionesQueLeGustanA xs u


---------------------------------------- EJERCICIO 8 ----------------------------------------

-- Dados una red y dos usuarios, si ambos le dieron "like" a las mismas publicaciones, devuelve True. Si no, devuelve False.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA (red) (u1)) (publicacionesQueLeGustanA (red) (u2))


---------------------------------------- EJERCICIO 9 ----------------------------------------

-- Dado un usuario, si este ha hecho publicaciones en la red social dada y existe algún otro usuario que le haya dado "like" a todas ellas, devuelve "True". En otro caso, devuelve "False".
-- El usuario autor de las publicaciones no puede ser su propio seguidor fiel.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u | (publicacionesDe red u) /= [] = auxiliarTieneUnSeguidorFiel (publicacionesDe red u) (quitar u (usuarios red))
                          | otherwise = False

auxiliarTieneUnSeguidorFiel :: [Publicacion] -> [Usuario]-> Bool
auxiliarTieneUnSeguidorFiel _ [] = False
auxiliarTieneUnSeguidorFiel [] _ = True
auxiliarTieneUnSeguidorFiel (p:ps) u = auxiliarTieneUnSeguidorFiel ps (eliminarNoRepetidos ((u) ++ (likesDePublicacion p)))


---------------------------------------- EJERCICIO 10 ---------------------------------------

-- Devuelve True si existe una cadena de amigos que empiece con el primer usuario dado, y termine con el segundo usuario dado. En otro caso, devuelve False.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = existeSecuenciaDeAmigosAuxiliar (relaciones red) u1 u2 (amigosDe red u1)

existeSecuenciaDeAmigosAuxiliar :: [Relacion] -> Usuario -> Usuario -> [Usuario]-> Bool
existeSecuenciaDeAmigosAuxiliar _ _ _ [] = False
existeSecuenciaDeAmigosAuxiliar red u1 u2 (f:fs) | sonAmigos red u1 u2 = True 
                                              | otherwise = existeSecuenciaDeAmigosAuxiliar (eliminarRelacionesDe red u1) f u2 (fs ++ (auxiliarAmigosDe f red))

eliminarRelacionesDe :: [Relacion] -> Usuario -> [Relacion]
eliminarRelacionesDe [] _ = []
eliminarRelacionesDe (r:rs) u | estaEnRelacion u r = eliminarRelacionesDe rs u
                              | otherwise = r : eliminarRelacionesDe rs u

sonAmigos :: [Relacion] -> Usuario -> Usuario -> Bool
sonAmigos [] _ _ = False
sonAmigos (r:rs) u u2 | estaEnRelacion u r && estaEnRelacion u2 r = True
                      | otherwise = sonAmigos rs u u2

estaEnRelacion :: Usuario -> Relacion -> Bool
estaEnRelacion u (r1, r2) = u == r1 || u == r2


---------------------------------------------------------------------------------------------

-- Funciones auxiliares generales

pertenece :: (Eq t) => t -> [t] -> Bool
-- Requiere: True
-- Dado un elemento e y una lista. Devuelve True si e pertenece a la lista. En otro caso, False.
pertenece _ [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
-- Requiere: True
-- Dada una lista, devuelve la lista resultante de elimina los elementos que se encontraban repetidos.
eliminarRepetidos s | todosDistintos s = s
eliminarRepetidos (x:xs) | pertenece x xs = x : (eliminarRepetidos (quitarTodos x xs))
                         | otherwise = x : (eliminarRepetidos xs)

eliminarNoRepetidos :: (Eq t) => [t] -> [t]
-- Requiere: True
-- Función que elimina elementos no repetidos, en caso de estar repetido quedarán n-1 repeticiones. Ej: [1,1,1] -> [1,1]
eliminarNoRepetidos s | todosDistintos s = []
eliminarNoRepetidos (x:xs) | not(pertenece x xs) = (eliminarNoRepetidos xs)
                           | otherwise = x : eliminarNoRepetidos xs

quitar :: (Eq t) => t -> [t] -> [t]
-- Requiere: True
-- Dado un elemento e y una lista. Elimina, de existir, la primera (solamente) aparición del elemento e en la lista.
quitar _ [] = []
quitar e (x:xs) | e == x = xs
                | otherwise = (x) : (quitar e xs)

quitarTodos :: (Eq t) => t -> [t] -> [t]
-- Requiere: True
-- Dado un elemento e y una lista. Elimina, de existir, todas las aparición del elemento e en la lista.
quitarTodos _ [] = []
quitarTodos e (x:xs) | e == x = (quitarTodos e xs)
                     | otherwise = (x) : (quitarTodos e xs)

todosDistintos :: (Eq t) => [t] -> Bool
-- Requiere: True
-- Dada una lista, devuelve True si no hay elementos repetidos en la lista. En otro caso, False.
todosDistintos [] = True
todosDistintos (x:xs) = (not (pertenece x xs)) && (todosDistintos xs)

esContenido :: (Eq t) => [t] -> [t] -> Bool
-- Requiere: True
-- Sean a, b listas. Devuelve True si a esta contenido en b, es decir, cada elemento de a esta también en b. En otro caso, False.
-- Las repeticiones y orden no afectan al resultado. Ej: esContenido [1,2,2,1,1] [1,2,3,2] -> True
esContenido [] b = True
esContenido (x:xs) b | pertenece x b = esContenido xs b
                     | otherwise = False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
-- Requiere: True
-- Sean a, b listas. Devuelve True si a tiene los mismos elementos que b. En otro caso, False.
-- Las repeticiones y orden no afectan al resultado. Ej: mismosElementos [1,2,3,3,2,1] [1,3,2] -> True
mismosElementos a b = (esContenido a b) && (esContenido b a)

longitud :: [t] -> Int
-- Requiere: True
-- Dada una lista, devuelve la cantidad de elementos que tiene la lista.
longitud [] = 0
longitud (x:xs) = (longitud xs) + 1
