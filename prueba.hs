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
        (estaRobertoCarlosTesteable4 (usuariosTodos, [relacion2_4, relacion1_3], [])) ~?= False,

--------------------------------------------------------------------------------------------

    -- EJERCICIO 6:

    " publicacionesDe 1: Usuario no tiene publicaciones" ~:
        (publicacionesDe (usuariosDel1_4, [], [pub3_a]) usuario1) ~?= [],

    " publicacionesDe 2: Usuario tiene una sola publicación" ~:
        (publicacionesDe (usuariosTodos, [], [pub1_b, pub4_a]) usuario4) ~?= [pub4_a],

    " publicacionesDe 3: Usuario tiene más (mayor estricto) de una publicación" ~:
        (publicacionesDe (usuariosDel1_4, [], [pub1_a, pub1_b, pub2_b, pub1_c]) usuario1) ~?= [pub1_a, pub1_b, pub1_c],

--------------------------------------------------------------------------------------------

    -- EJERCICIO 7:

    " publicacionesQueLeGustanA 1: A usuario no le gusta ninguna publicación" ~:
        (publicacionesQueLeGustanA (usuariosTodos, [], [pub1_b, pub4_c]) usuario1) ~?= [],

    " publicacionesQueLeGustanA 2: A usuario le gustan algunas publicaciones de varios usuarios distintos" ~:
        (publicacionesQueLeGustanA (usuariosTodos, [], [pub1_a, pub1_d, pub1_e, pub2_a, pub2_b]) usuario3) ~?= [pub1_a, pub1_e, pub2_b],

    " publicacionesQueLeGustanA 3: A usuario le gustan sus propias publicaciones" ~:
        (publicacionesQueLeGustanA (usuariosTodos, [], [pubAutoLike1_a, pubAutoLike1_b]) usuario1) ~?= [pubAutoLike1_a, pubAutoLike1_b],

    " publicacionesQueLeGustanA 4: A usuario le gusta publicaciones que tienen la misma string pero distinto autor" ~:
        (publicacionesQueLeGustanA (usuariosTodos, [], [pubMismaString3, pubMismaString4]) usuario6) ~?= [pubMismaString3, pubMismaString4],

--------------------------------------------------------------------------------------------

    -- EJERCICIO 8:

    " lesGustanLasMismasPublicaciones 1: A ninguno de los dos le gustan ninguna publicación" ~:
        (lesGustanLasMismasPublicaciones (usuariosTodos, [], [pub1_c, pub3_b]) usuario1 usuario2) ~?= True,

    " lesGustanLasMismasPublicaciones 2: A los dos les gustan exactamente las mismas publicaciones" ~:
        (lesGustanLasMismasPublicaciones (usuariosTodos, [], [pub2_b, pub3_c, pub4_c]) usuario1 usuario3) ~?= True,

    " lesGustanLasMismasPublicaciones 3: Likes de u1 ⊆ likes de u2 pero likes de u2 ⊄ likes de u1, por lo tanto, likes de u1 != likes de u2" ~:
        (lesGustanLasMismasPublicaciones (usuariosTodos, [], [pub2_b, pub3_c, pub1_a, pub4_c]) usuario1 usuario3) ~?= False,

    " lesGustanLasMismasPublicaciones 4: No tienen ningún like en común" ~:
        (lesGustanLasMismasPublicaciones (usuariosTodos, [], [pub1_a, pub1_c, pub3_b]) usuario2 usuario4) ~?= False,

    " lesGustanLasMismasPublicaciones 5: Tienen en común algunos likes pero no todos, por lo tanto, likes a ⊄ likes b y likes b ⊄ likes a" ~:
        (lesGustanLasMismasPublicaciones (usuariosTodos, [], [pub2_b, pub3_c, pub4_c, pub4_a, pub1_e]) usuario1 usuario3) ~?= False,

    " lesGustanLasMismasPublicaciones 6: Ambos usuarios son la misma persona" ~:
        (lesGustanLasMismasPublicaciones (usuariosTodos, [], [pub2_b, pub3_c, pub1_a, pub4_c]) usuario1 usuario1) ~?= True

--------------------------------------------------------------------------------------------

    -- EJERCICIO 9:
    
    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

{-

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

-- Publicaciones:

pub1_a = (usuario1, "Este es mi primer post", [usuario2, usuario3])
pub1_b = (usuario1, "Este es mi segundo post", [usuario4])
pub1_c = (usuario1, "Este es mi tercer post", [usuario4, usuario5])
pub1_d = (usuario1, "Este es mi cuarto post", [])
pub1_e = (usuario1, "Este es como mi quinto post", [usuario3])

pubAutoLike1_a = (usuario1, "Me amo, como la Tierra al Sol", [usuario1])
pubAutoLike1_b = (usuario1, "Me amo, como Narciso soy", [usuario1, usuario2])

pub2_a = (usuario2, "Hello World", [usuario2])
pub2_b = (usuario2, "Good Bye World", [usuario1, usuario3])

pub3_a = (usuario3, "Lorem Ipsum", [])
pub3_b = (usuario3, "dolor sit amet", [usuario4])
pub3_c = (usuario3, "consectetur adipiscing elit", [usuario1, usuario3])

pub4_a = (usuario4, "I am Alice. Not", [usuario1, usuario5])
pub4_b = (usuario4, "I am Bob", [])
pub4_c = (usuario4, "Just kidding, i am Rocio", [usuario5, usuario6])

pubMismaString3 = (usuario3, "Tengo el mismo texto", [usuario6])
pubMismaString4 = (usuario4, "Tengo el mismo texto", [usuario6, usuario2])

usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [pub1_a, pub1_b, pub2_a, pub2_b, pub3_a, pub3_b, pub4_a, pub4_b]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [pub1_c, pub1_d, pub1_e, pub3_a, pub3_b, pub3_c]
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

usuariosDel1_4 = [usuario1, usuario2, usuario3, usuario4]