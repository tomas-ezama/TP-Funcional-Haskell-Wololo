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
nombresDeUsuarios x = proyectarNombres(usuarios x)

proyectarNombres :: [Usuario] -> [String]
-- Proyectar nombres sin repetidos.
proyectarNombres us = eliminarRepetidos (proyectarNombresConRepetidos (us))

proyectarNombresConRepetidos :: [Usuario] -> [String]
proyectarNombresConRepetidos [] = []
proyectarNombresConRepetidos (x:xs) = nombreDeUsuario x : proyectarNombresConRepetidos xs


---------------------------------------- EJERCICIO 2 ----------------------------------------

-- Devuelve una lista con todos los amigos del usuario dado en la red social dada.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe r u = proyectarNombresDeAmigosDe u (relaciones r)

proyectarNombresDeAmigosDe :: Usuario -> [Relacion] -> [Usuario]
proyectarNombresDeAmigosDe _ [] = []
proyectarNombresDeAmigosDe u ((x, y):xs) | u == x = y : proyectarNombresDeAmigosDe u xs 
                                         | u == y = x : proyectarNombresDeAmigosDe u xs
                                         | otherwise = proyectarNombresDeAmigosDe u xs


---------------------------------------- EJERCICIO 3 ----------------------------------------

-- Devuelve la cantidad de amigos del usuario dado en la red social dada.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usuario = longitud (amigosDe red usuario)


---------------------------------------- EJERCICIO 4 ----------------------------------------

-- Devuelve al usuario con más amigos, en la red social dada
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = auxiliarUsuarioConMasAmigos (red) (usuarios (red))

auxiliarUsuarioConMasAmigos :: RedSocial -> [Usuario] -> Usuario
auxiliarUsuarioConMasAmigos red [u] = u
auxiliarUsuarioConMasAmigos red (x:y:xs) | cantidadDeAmigos red x >= cantidadDeAmigos red y = auxiliarUsuarioConMasAmigos red (x:xs)
                                         | otherwise = auxiliarUsuarioConMasAmigos red (y:xs)


---------------------------------------- EJERCICIO 5 ----------------------------------------

-- Devuelve "True" si existe un usuario, en la red social dada, que tenga más de 1000000 amigos. En otro caso, devuelve "False".
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = cantidadDeAmigos (red) (usuarioConMasAmigos (red)) > 1000000

-- Función auxiliar para testear estaRobertoCarlos. Utiliza 4 en vez de 1000000.
estaRobertoCarlosTesteable4 :: RedSocial -> Bool
estaRobertoCarlosTesteable4 red = cantidadDeAmigos (red) (usuarioConMasAmigos (red)) > 4


---------------------------------------- EJERCICIO 6 ----------------------------------------

-- Devuelve una lista con todas las publicaciones hechas por el usuario dado, en la red social dada
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = auxiliarPublicacionesDe (publicaciones (red)) (u)

auxiliarPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
-- Nota temporal: Le paso como input (publicaciones (red))
auxiliarPublicacionesDe [] _ = []
--auxiliarPublicacionesDe (x:xs) u = if (usuarioDePublicacion (x)) == u then (x) : auxiliarPublicacionesDe xs u else auxiliarPublicacionesDe xs u
auxiliarPublicacionesDe (x:xs) u | usuarioDePublicacion (x) == u = (x) : auxiliarPublicacionesDe xs u
                                 | otherwise = auxiliarPublicacionesDe xs u


---------------------------------------- EJERCICIO 7 ----------------------------------------

-- Devuelve una lista con todas las publicaciones "likeadas" por el usuario dado, en la red social dada.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = auxiliarPublicacionesQueLeGustanA (publicaciones (red)) (u)

auxiliarPublicacionesQueLeGustanA :: [Publicacion] -> Usuario -> [Publicacion]
-- Nota temporal: Le paso como input (publicaciones (red))
auxiliarPublicacionesQueLeGustanA [] _ = []
auxiliarPublicacionesQueLeGustanA (x:xs) u | pertenece (u) (likesDePublicacion (x)) = (x) : auxiliarPublicacionesQueLeGustanA xs u
                                           | otherwise = auxiliarPublicacionesQueLeGustanA xs u
-- OBS temporal: auxiliarPublicacionesDe y auxiliarPublicacionesQueLeGustanA tienen el mismo código cambiando la función que utilizan


---------------------------------------- EJERCICIO 8 ----------------------------------------

-- Si ambos usuarios dados, en la red social dada, le dieron "like" a las mismas publicaciones, devuelve "True". Si no, devuelve "False".
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA (red) (u1)) (publicacionesQueLeGustanA (red) (u2))


-- Dado un usuario, si este ha hecho publicaciones en la red social dada y existe algún otro usuario que le haya dado "like" a todas ellas, devuelve "True". En otro caso, devuelve "False".
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u | (publicacionesDe r u) /= [] = auxiliarTieneUnSeguidorFiel (publicacionesDe r u) (quitar u (usuarios r))
                        | otherwise = False


---------------------------------------- EJERCICIO 9 ----------------------------------------

auxiliarTieneUnSeguidorFiel :: [Publicacion] -> [Usuario]-> Bool
auxiliarTieneUnSeguidorFiel _ [] = False
auxiliarTieneUnSeguidorFiel [] _ = True
auxiliarTieneUnSeguidorFiel (p:ps) u = auxiliarTieneUnSeguidorFiel ps (eliminarNoRepetidos ((u) ++ (likesDePublicacion p)))


---------------------------------------- EJERCICIO 10 ---------------------------------------

-- Devuelve "True" si existe una cadena de amigos que empiece con el primer usuario dado, y termine con el segundo usuario dado. En otro caso, devuelve "false".
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 | u1 == u2 && (cantidadDeAmigos (red) (u1)) > 0 = True
existeSecuenciaDeAmigos red u1 u2 | u1 == u2 = False
existeSecuenciaDeAmigos red u1 u2 = auxiliarExisteSecuenciaDeAmigos red [u1] u2 []

-- PRIMER PASO: RAMIFICAR LOS AMIGOS DE LOS AMIGOS. (AMIGOS DE AMIGOS)
amigosDeUsuariosConRepetidos :: RedSocial -> [Usuario] -> [Usuario]
-- Toma una red social y una lista de usuarios. Devuelve una lista con todos los amigos de la lista de usuarios. (Tiene repertidos)
-- o sea Devuelve una lista con todos los usuarios que son amigos con al menos un usuario de la lista
amigosDeUsuariosConRepetidos _ [] = []
amigosDeUsuariosConRepetidos red (x:xs) = (amigosDe (red) (x)) ++ (amigosDeUsuariosConRepetidos (red) (xs))

-- SEGUNDO PASO: LE SACO LOS REPETIDOS.
amigosDeUsuarios :: RedSocial -> [Usuario] -> [Usuario]
-- Toma una red social y una lista de usuarios. Devuelve una lista con todos los usuarios que son amigos con al menos un usuario de la lista. (Sin repeticiones)
amigosDeUsuarios red us = eliminarRepetidos(amigosDeUsuariosConRepetidos red us)

-- TERCERO: Crear lista negra
-- CUARTO PASO: lista blanca - lista negra
auxiliarExisteSecuenciaDeAmigos :: RedSocial -> [Usuario] -> Usuario -> [Usuario] -> Bool
auxiliarExisteSecuenciaDeAmigos red [] _ _ = False
auxiliarExisteSecuenciaDeAmigos red us u_objetivo lista_negra | pertenece (u_objetivo) (us) = True 
        | otherwise = auxiliarExisteSecuenciaDeAmigos (red) (restaListas (amigosDeUsuarios (red) (us)) (lista_negra)) (u_objetivo) (lista_negra ++ us)


-- Funciones auxiliares generales

pertenece :: (Eq t) => t -> [t] -> Bool
-- Requiere: True
pertenece _ [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
-- Requiere: True
eliminarRepetidos s | todosDistintos s = s
eliminarRepetidos (x:xs) | pertenece x xs = x : (eliminarRepetidos (quitarTodos x xs))
                         | otherwise = x : (eliminarRepetidos xs)


eliminarNoRepetidos :: (Eq t) => [t] -> [t]
-- Funcion elimina elementos no repetidos en caso de estar repetido quedaran n-1 repeticiones. [1,1,1] -> [1,1]
-- Requiere: True
eliminarNoRepetidos s | todosDistintos s = []
eliminarNoRepetidos (x:xs) | not(pertenece x xs) = (eliminarNoRepetidos xs)
                           | otherwise = x : eliminarNoRepetidos xs

quitar :: (Eq t) => t -> [t] -> [t]
-- Requiere: True
-- Elimina, de existir, la primera (solamente) aparición del elemento e.
quitar _ [] = []
quitar e (x:xs) | e == x = xs
                | otherwise = (x) : (quitar e xs)

quitarTodos :: (Eq t) => t -> [t] -> [t]
-- Requiere: True
quitarTodos _ [] = []
quitarTodos e (x:xs) | e == x = (quitarTodos e xs)
                     | otherwise = (x) : (quitarTodos e xs)

todosDistintos :: (Eq t) => [t] -> Bool
-- Requiere: True
todosDistintos [] = True
todosDistintos (x:xs) = (not (pertenece x xs)) && (todosDistintos xs)

esContenido :: (Eq t) => [t] -> [t] -> Bool
-- Requiere: True
-- Quiero ver que a contenido en b. o sea cada elemento de a esta también en b.
esContenido [] b = True
esContenido (x:xs) b | pertenece x b = esContenido xs b
                     | otherwise = False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
-- Requiere: True
mismosElementos s r = (esContenido s r) && (esContenido r s)


restaListas :: (Eq t) => [t] -> [t] -> [t]
-- Requiere: True
-- A - B son los x tales que x pertenece a A y x no pertenece a b.
-- Devuelve el resultado restante en el orden en el que aparecian los elementos en A. 
restaListas [] _ = []
restaListas (x:xs) lista_b | pertenece x lista_b = restaListas xs lista_b
                           | otherwise = x : (restaListas xs lista_b)

longitud :: [t] -> Int
-- Requiere: True
longitud [] = 0
longitud (x:xs) = (longitud xs) + 1
