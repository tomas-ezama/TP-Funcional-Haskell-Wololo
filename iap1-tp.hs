-- Completar con los datos del grupo
--
-- Nombre de Grupo: wololo
-- Integrante 1: Tomas Ezama, tomasezama@gmail.com, 475/23 
-- Integrante 2: Eduardo Baars, eduardobaars@id.uff.br, 1338/21
-- Integrante 3: Mauricio Romero Laino, mauricioromerolaino@gmail.com, 18/23
-- Integrante 4: Cian Andrés Bautista, andycia802@gmail.com, 937/21

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

-- Devuelve una lista con todos los usuarios de la red social dada.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios x = proyectarNombres(usuarios x)

proyectarNombres :: [Usuario] -> [String]
-- Proyectar nombres sin repetidos.
proyectarNombres us = eliminarRepetidos (proyectarNombresConRepetidos (us))

proyectarNombresConRepetidos :: [Usuario] -> [String]
proyectarNombresConRepetidos [] = []
proyectarNombresConRepetidos (x:xs) = nombreDeUsuario x : proyectarNombresConRepetidos xs

-- Devuelve una lista con todos los amigos del usuario dado en la red social dada.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe r u = proyectarNombresDeAmigosDe u (relaciones r)

proyectarNombresDeAmigosDe :: Usuario -> [Relacion] -> [Usuario]
proyectarNombresDeAmigosDe _ [] = []
proyectarNombresDeAmigosDe u ((x, y):xs) | u == x = y : proyectarNombresDeAmigosDe u xs 
                                         | u == y = x : proyectarNombresDeAmigosDe u xs
                                         | otherwise = proyectarNombresDeAmigosDe u xs

-- Devuelve la cantidad de amigos del usuario dado en la red social dada.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos r u = contarAmigosDe (relaciones r) u

contarAmigosDe :: [Relacion] -> Usuario -> Int
contarAmigosDe [] _ = 0
contarAmigosDe ((x, y):xs) u | u == x = 1 + contarAmigosDe xs u
                             | u == y = 1 + contarAmigosDe xs u
                             | otherwise = 0 + contarAmigosDe xs u

-- Devuelve al usuario con más amigos, en la red social dada
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos r = proyectarUsuarioConMasAmigos (usuarios r) (relaciones r)

proyectarUsuarioConMasAmigos :: [Usuario] -> [Relacion] -> Usuario
proyectarUsuarioConMasAmigos [x] _ = x
proyectarUsuarioConMasAmigos (x:y:xs) r | contarAmigosDe r x >= contarAmigosDe r y = proyectarUsuarioConMasAmigos (x:xs) r
                                        | otherwise = proyectarUsuarioConMasAmigos (y:xs) r
                                
-- Devuelve "True" si existe un usuario, en la red social dada, que tenga más de 1000000 amigos. En otro caso, devuelve "False".
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = cantidadDeAmigos (red) (usuarioConMasAmigos (red)) > 1000000

-- Devuelve una lista con todas las publicaciones hechas por el usuario dado, en la red social dada
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = auxiliarPublicacionesDe (publicaciones (red)) (u)

auxiliarPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
-- Nota temporal: Le paso como input (publicaciones (red))
auxiliarPublicacionesDe [] _ = []
--auxiliarPublicacionesDe (x:xs) u = if (usuarioDePublicacion (x)) == u then (x) : auxiliarPublicacionesDe xs u else auxiliarPublicacionesDe xs u
auxiliarPublicacionesDe (x:xs) u | usuarioDePublicacion (x) == u = (x) : auxiliarPublicacionesDe xs u
                                 | otherwise = auxiliarPublicacionesDe xs u


-- Devuelve una lista con todas las publicaciones "likeadas" por el usuario dado, en la red social dada.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = auxiliarPublicacionesQueLeGustanA (publicaciones (red)) (u)

auxiliarPublicacionesQueLeGustanA :: [Publicacion] -> Usuario -> [Publicacion]
-- Nota temporal: Le paso como input (publicaciones (red))
auxiliarPublicacionesQueLeGustanA [] _ = []
auxiliarPublicacionesQueLeGustanA (x:xs) u | pertenece (u) (likesDePublicacion (x)) = (x) : auxiliarPublicacionesQueLeGustanA xs u
                                           | otherwise = auxiliarPublicacionesQueLeGustanA xs u
-- OBS temporal: auxiliarPublicacionesDe y auxiliarPublicacionesQueLeGustanA tienen el mismo código cambiando la función que utilizan


-- Si ambos usuarios dados, en la red social dada, le dieron "like" a las mismas publicaciones, devuelve "True". Si no, devuelve "False".
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA (red) (u1)) (publicacionesQueLeGustanA (red) (u2))


-- Dado un usuario, si este ha hecho publicaciones en la red social dada y existe algún otro usuario que le haya dado "like" a todas ellas, devuelve "True". En otro caso, devuelve "False".
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u = if (publicacionesDe r u) /= [] then auxiliarTieneUnSeguidorFiel (publicacionesDe r u) (usuarios r) else False

auxiliarTieneUnSeguidorFiel :: [Publicacion] -> [Usuario]-> Bool
auxiliarTieneUnSeguidorFiel _ [] = False
auxiliarTieneUnSeguidorFiel [] _ = True
auxiliarTieneUnSeguidorFiel (p:ps) u = auxiliarTieneUnSeguidorFiel ps (eliminarNoRepetidos ((u) ++ (likesDePublicacion p)))



-- Devuelve "True" si existe una cadena de amigos que empiece con el primer usuario dado, y termine con el segundo usuario dado. En otro caso, devuelve "false".
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos r u1 u2 = existeSecuenciaDeAmigosAuxiliar (relaciones r) u1 u2 (amigosDe r u1)

existeSecuenciaDeAmigosAuxiliar :: [Relacion] -> Usuario -> Usuario -> [Usuario]-> Bool
existeSecuenciaDeAmigosAuxiliar _ _ _ [] = False
existeSecuenciaDeAmigosAuxiliar r u u2 (f:fs) | sonAmigos r u u2 = True 
                                              | otherwise = existeSecuenciaDeAmigosAuxiliar (eliminarRelacionesDe r u) f u2 (fs ++ (proyectarNombresDeAmigosDe f r))

eliminarRelacionesDe :: [Relacion] -> Usuario -> [Relacion]
eliminarRelacionesDe [] _ = []
eliminarRelacionesDe (r:rs) u | estaEnRelacion u r = eliminarRelacionesDe rs u
                              | otherwise = r : eliminarRelacionesDe rs u

sonAmigos :: [Relacion] -> Usuario -> Usuario -> Bool
sonAmigos [] _ _ = False
sonAmigos (r:rs) u u2 | estaEnRelacion u r && estaEnRelacion u2 r = True
                      | otherwise = sonAmigos rs u u2
-- Idea:
{-
existeSecuenciaDeAmigos red u1 u2
    | u1 se relaciona con u2 = True                                                                                 -- O sea, (u1, u2) existe
    | (relaciones de u1) se relaciona con u2 = True
    | otherwise = existeSecuenciaDeAmigos red u1 u2 = existeSecuenciaDeAmigosAux red u1 u1 u2

existeSecuenciaDeAmigosAux :: RedSocial -> Usuario -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigosAux red u1 uX u2
    | longitud (amigosDeAQueNoSeanBOCadenaDeAmigosDeB uX u1) == 0 = False
    | uX se relaciona con u2 = True                                                                                 -- O sea, (uX, u2) existe
    | otherwise = existeSecuenciaDeAmigosAux red u1 (Todos los uY tales que Relaciones (uX, uY) existen) u2
    
amigosDeAQueNoSeanBOCadenaDeAmigosDeB :: ...
-}


-- Funciones varias

pertenece :: (Eq t) => t -> [t] -> Bool
-- Requiere: True
pertenece _ [] = False
pertenece e (x:xs) = if e == x then True else pertenece e xs

estaEnRelacion :: Usuario -> Relacion -> Bool
estaEnRelacion u (r1, r2) = u == r1 || u == r2

eliminarRepetidos :: (Eq t) => [t] -> [t]
-- Requiere: True
eliminarRepetidos s | todosDistintos s = s  
eliminarRepetidos (x:xs) = (x) : (if pertenece x xs then (eliminarRepetidos (quitarTodos x xs)) else (eliminarRepetidos xs))

eliminarNoRepetidos :: (Eq t) => [t] -> [t]
-- Funcion elimina elementos no repetidos en caso de estar repetido quedaran n-1 repeticiones. [1,1,1] -> [1,1]
-- Requiere: True
eliminarNoRepetidos s | todosDistintos s = []
eliminarNoRepetidos (x:xs) = (if not(pertenece x xs) then (eliminarNoRepetidos xs) else (x : eliminarNoRepetidos xs))

quitarTodos :: (Eq t) => t -> [t] -> [t]
-- Requiere: True
quitarTodos _ [] = []
quitarTodos e (x:xs) = if e == x then (quitarTodos e xs) else (x) : (quitarTodos e xs)

todosDistintos :: (Eq t) => [t] -> Bool
-- Requiere: True
todosDistintos [] = True
todosDistintos (x:xs) = (not (pertenece x xs)) && (todosDistintos xs)

esContenido :: (Eq t) => [t] -> [t] -> Bool
-- Requiere: True
-- Quiero ver que a contenido en b. o sea cada elemento de a esta también en b.
esContenido [] b = True
esContenido (x:xs) b = if pertenece x b then esContenido xs b else False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
-- Requiere: True
mismosElementos s r = (esContenido s r) && (esContenido r s)
