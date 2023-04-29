-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Tomas Ezama, tomasezama@gmail.com, 475/23 
-- Integrante 2: Eduardo Baars, eduardobaars@id.uff.br, 1338/21
-- Integrante 3: Mauricio Romero Laino, mauricioromerolaino@gmail.com, 18/23
-- Integrante 4: Nombre Apellido, email, LU

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

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios x = proyectarNombres(usuarios x)

proyectarNombres :: [Usuario] -> [String]
-- Proyectar nombres sin repetidos
proyectarNombres us = eliminarRepetidos (proyectarNombresConRepetidos (us))

proyectarNombresConRepetidos :: [Usuario] -> [String]
proyectarNombresConRepetidos [] = []
proyectarNombresConRepetidos (x:xs) = nombreDeUsuario x : proyectarNombresConRepetidos xs

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe r u = proyectarNombresDeAmigosDe u (relaciones r)

proyectarNombresDeAmigosDe :: Usuario -> [Relacion] -> [Usuario]
proyectarNombresDeAmigosDe _ [] = []
proyectarNombresDeAmigosDe u ((x, y):xs) | u == x = y : proyectarNombresDeAmigosDe u xs 
                                         | u == y = x : proyectarNombresDeAmigosDe u xs
                                         | otherwise = proyectarNombresDeAmigosDe u xs

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos r u = contarAmigosDe (relaciones r) u

contarAmigosDe :: [Relacion] -> Usuario -> Int
contarAmigosDe [] _ = 0
contarAmigosDe ((x, y):xs) u | u == x = 1 + contarAmigosDe xs u
                             | u == y = 1 + contarAmigosDe xs u
                             | otherwise = 0 + contarAmigosDe xs u

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos r = proyectarUsuarioConMasAmigos (usuarios r) (relaciones r)

proyectarUsuarioConMasAmigos :: [Usuario] -> [Relacion] -> Usuario
proyectarUsuarioConMasAmigos [x] _ = x
proyectarUsuarioConMasAmigos (x:y:xs) r | contarAmigosDe r x >= contarAmigosDe r y = proyectarUsuarioConMasAmigos (x:xs) r
                                        | otherwise = proyectarUsuarioConMasAmigos (y:xs) r
                                
-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = cantidadDeAmigos (red) (usuarioConMasAmigos (red)) > 1000000

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined




-- Funciones varias

pertenece :: (Eq t) => t -> [t] -> Bool
-- Requiere: True
pertenece _ [] = False
pertenece e (x:xs) = if e == x then True else pertenece e xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
-- Requiere: True
eliminarRepetidos s | todosDistintos s = s  
eliminarRepetidos (x:xs) = (x) : (if pertenece x xs then (eliminarRepetidos (quitarTodos x xs)) else (eliminarRepetidos xs))

quitarTodos :: (Eq t) => t -> [t] -> [t]
-- Requiere: True
quitarTodos _ [] = []
quitarTodos e (x:xs) = if e == x then (quitarTodos e xs) else (x) : (quitarTodos e xs)

todosDistintos :: (Eq t) => [t] -> Bool
-- Requiere: True
todosDistintos [] = True
todosDistintos (x:xs) = (not (pertenece x xs)) && (todosDistintos xs)