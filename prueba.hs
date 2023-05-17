import Test.HUnit
import Solucion

------- RECORDATORIO: BORRAR Y CAMBIAR LO QUE TENGA "redA" !!!!!!!

main = runTestTT tests

tests = test [

    -- EJERCICIO 1:

    " nombresDeUsuarios 1: Lista de usuarios vacía" ~:
        (nombresDeUsuarios redVacia) ~?= [],

    " nombresDeUsuarios 2: Lista de usuarios con nombres repetidos e id distintas" ~:
        (nombresDeUsuarios (hayUsuariosConElMismoNombre, [], [])) ~?= ["Antonella", "Mauricio"],

    " nombresDeUsuarios 3: Lista de usuarios sin nombres repetidos (lista no vacía)" ~:
        (nombresDeUsuarios redA) ~?= ["Mauricio", "Andres", "Tomas", "Rocio"],

--------------------------------------------------------------------------------------------

    -- EJERCICIO 2:

    " amigosDe 1: Usuario sin amigos" ~:
        (amigosDe redUsuario2SinAmigos usuario2) ~?= [],

    " amigosDe 2: Usuario tiene amigos con nombres repetidos e id distintas" ~:
        (amigosDe (hayUsuariosConElMismoNombre, [relacion1_5, relacion1_6], []) usuario1) ~?= [usuario5, usuario6],

    " amigosDe 3: Usuario no tiene amigos con nombres repetidos" ~:
        (amigosDe redA usuario1) ~?= [usuario2, usuario4],

--------------------------------------------------------------------------------------------

    -- EJERCICIO 3:

    " cantidadDeAmigos 1: No tiene amigos" ~:
        (cantidadDeAmigos (usuariosTodos, [relacion2_3, relacion2_4], []) usuario1) ~?= 0,

    " cantidadDeAmigos 2: Tiene un único un amigo" ~:
        (cantidadDeAmigos (usuariosTodos, [relacion2_4, relacion1_3], []) usuario3) ~?= 1,

    " cantidadDeAmigos 3: Tiene más de un amigo" ~:
        (cantidadDeAmigos redUsuario1Con5Amigos usuario1) ~?= 5,

--------------------------------------------------------------------------------------------

    -- EJERCICIO 4:

    " usuarioConMasAmigos 1: Red con un solo usuario. Sin amigos" ~:
        (usuarioConMasAmigos ([usuario4], [], [])) ~?= usuario4,

    " usuarioConMasAmigos 2: Dos usuarios con la misma cantidad de amigos" ~:
        expectAny (usuarioConMasAmigos (usuariosTodos, [relacion1_2, relacion1_5, relacion2_3, relacion4_5, relacion2_5], [])) [usuario2, usuario5],

    " usuarioConMasAmigos 3: Usuario con mayor estricto cantidad de amigos que el resto" ~:
        (usuarioConMasAmigos redUsuario1Con5Amigos) ~?= usuario1,

    -- el 4 es innecesario tal vez.
    " usuarioConMasAmigos 4: Usuario con mayor estricto cantidad de amigos que el resto y el resto tiene números variados" ~:
        (usuarioConMasAmigos (usuariosTodos, [relacion1_4, relacion2_4, relacion1_3, relacion1_5], [])) ~?= usuario1,

--------------------------------------------------------------------------------------------

    -- EJERCICIO 5:
{-  Para simplificar el testing, utilizaremos estaRobertoCarlosTesteable4 que remplaza el 1000000 de amigos requeridos
    en el enunciado por un número más manejable, en nuestro caso utilizaremos al 4. -}

    " estaRobertoCarlos 1: cantidadDeAmigos > n" ~:
        (estaRobertoCarlosTesteable4 redUsuario1Con5Amigos) ~?= True,

    " estaRobertoCarlos 2: cantidadDeAmigos = n" ~:
        (estaRobertoCarlosTesteable4 redUsuario2Con4Amigos) ~?= False,

    " estaRobertoCarlos 3: cantidadDeAmigos < n" ~:
        (estaRobertoCarlosTesteable4 (usuariosTodos, [relacion2_4, relacion1_3], [])) ~?= False

--------------------------------------------------------------------------------------------

{-
    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True -}
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Datos de las posibles redes sociales:

-- Usuarios:
usuario1 = (1, "Mauricio")
usuario2 = (2, "Andres")
usuario3 = (3, "Tomas")
usuario4 = (4, "Rocio")
usuario5 = (5, "Antonella")
usuario6 = (6, "Antonella")

-- Relaciones posibles:
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1)
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)

relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion2_5 = (usuario2, usuario5)
relacion2_6 = (usuario2, usuario6)

relacion3_4 = (usuario3, usuario4)
relacion3_5 = (usuario3, usuario5)
relacion3_6 = (usuario3, usuario6)

relacion4_5 = (usuario4, usuario5)
relacion4_6 = (usuario4, usuario6)

relacion5_6 = (usuario5, usuario6)

-- Publicaciones: RECORDATORIO: MODIFICAR!!
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Rocio", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

-------------------------------------


relacionesUsuario2SinAmigos = [relacion1_4, relacion3_4]
redUsuario2SinAmigos = (usuariosA, relacionesUsuario2SinAmigos, [])

usuariosVacio = []
relacionesVacio = []
publicacionesVacio = []
redVacia = (usuariosVacio, relacionesVacio, publicacionesVacio)

hayUsuariosConElMismoNombre = [usuario6, usuario1, usuario5]

usuariosRepetidos = [usuario1, usuario2, usuario1, usuario2]

usuariosTodos = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6]


relacionesUsuario1AmigoDeTodos = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6] -- Usuario 1 tiene 5 amigos.

redUsuario1Con5Amigos = (usuariosTodos, relacionesUsuario1AmigoDeTodos, [])

relacionesUsuario2Con4Amigos = [relacion1_2, relacion2_3, relacion2_4, relacion2_5]

redUsuario2Con4Amigos = (usuariosTodos, relacionesUsuario2Con4Amigos, [])
