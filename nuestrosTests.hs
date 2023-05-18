import Test.HUnit
import Solucion

-- Casos de Test

main = runTestTT todosLosTests
todosLosTests = test [run1, run2, run3, run4, run5, run6, run7, run8, run9, run10]

run1 = testEjercicio1
run2 = testEjercicio2
run3 = testEjercicio3
run4 = testEjercicio4
run5 = testEjercicio5
run6 = testEjercicio6
run7 = testEjercicio7
run8 = testEjercicio8
run9 = testEjercicio9
run10 = testEjercicio10




testEjercicio1 = test [
    "nombresDeUsuarios 1: Lista de usuarios vacía"         ~: nombresDeUsuarios ([],[],[])                 ~?= [],
    "nombresDeUsuarios 2: Lista de usuarios con repetidos" ~: nombresDeUsuarios ([u2, u3, u2],[],[]) ~?= ["Mauri", "Andy"],
    "nombresDeUsuarios 3: Lista de usuarios sin repetidos" ~: nombresDeUsuarios redTigres                       ~?= ["Tomi", "Mauri", "Andy"]
    ]

testEjercicio2 = test [
    "amigosDe 1: u no tiene amigos" ~: amigosDe red1us1pub u4 ~?= [],
    "amigosDe 2: u tiene amigos CON nombres repetidos ∧ IDs distintos" ~: amigosDe ([(1, "Tomi"), (2, "u"), (727, "Tomi")],[((2, "u"), (1,"Tomi")),((2, "u"), (727,"Tomi"))], []) (2, "u") ~?= [(1, "Tomi"),(727, "Tomi")],
    "amigosDe 3: u tiene amigos, pero SIN nombres repetidos" ~: amigosDe redTigres u3 ~?= [(2, "Mauri")]
    ]
    
testEjercicio3 = test [
    "cantidadDeAmigos 1: u no tiene amigos :("                                                             ~: cantidadDeAmigos red1us1pub u4 ~?= 0,
    "cantidadDeAmigos 2: u tiene un solo amigo"                                                            ~: cantidadDeAmigos redTigres u1 ~?= 1,
    "cantidadDeAmigos 3: u tiene más (mayor estricto) de un amigo (tiene una cantidad finita n de amigos)" ~: cantidadDeAmigos redTigres u2 ~?= 2
    ]

testEjercicio4 = test [
    "usuarioConMasAmigos 1: La red tiene un solo usuario y ninguna relación." ~: usuarioConMasAmigos red1us1pub ~?= u4,
    "usuarioConMasAmigos 2: Dos usuarios tienen la misma cantidad de amigos"  ~: expectAny (usuarioConMasAmigos redTigresRep) [u1, u2, u3],
    "usuarioConMasAmigos 3: Un usuario tiene una cantidad de amigos mayor estricta a la cantidad de amigos de los demás usuarios" ~: usuarioConMasAmigos redTigres ~?= u2
    ]

testEjercicio5 = test [
{-
Para simplificar el testing utilizaremos estaRobertoCarlosTesteable4, que remplaza el 1000000 de amigos requeridos
en el enunciado por un número más manejable. En nuestro caso, utilizaremos al 4.
-}
    "estaRobertoCarlos 1: cantidadDeAmigos > 4" ~: estaRobertoCarlosTesteable4 redRoberto1 ~?= True,
    "estaRobertoCarlos 2: cantidadDeAmigos = 4" ~: estaRobertoCarlosTesteable4 redRoberto2 ~?= False,
    "estaRobertoCarlos 3: cantidadDeAmigos < 4" ~: estaRobertoCarlosTesteable4 redTigres ~?= False
    ]
   
testEjercicio6 = test [
    "publicacionesDe 1: u no tiene publicaciones"                        ~: publicacionesDe red1us1pub u4 ~?= [],
    "publicacionesDe 2: u tiene una sola publicación"                    ~: publicacionesDe redTigresRep u1 ~?= [pub1_1],
    "publicacionesDe 3: u tiene más (mayor estricto) de una publicación" ~: publicacionesDe redTigres u3 ~?= [pub3_1, pub3_2, pub3_3]
    ]
   
testEjercicio7 = test [
    "publicacionesQueLeGustanA 1: A u no le gusta ninguna publicación"                       ~: publicacionesQueLeGustanA red1us1pub u4 ~?= [],
    "publicacionesQueLeGustanA 2: (∀n>=2) (Pertenece u, pub_n[1]) ∧ (pub_n[1] = pub_n-1[1])" ~: publicacionesQueLeGustanA redTigresRepCadena u1 ~?= [pub2_3, pub3_1],
    "publicacionesQueLeGustanA 3: Hay publicaciones con un mismo autor que le gustan a u"    ~: publicacionesQueLeGustanA redTigresRepCadena u3 ~?= [pub1_1, pub1_2, pub2_1, pub2_2],
    "publicacionesQueLeGustanA 4: A u solo le gustan sus propias publicaciones"              ~: publicacionesQueLeGustanA redIbai u5 ~?= [pub5_1, pub5_2, pub5_3]
    ]
   
testEjercicio8 = test [
    "lesGustanLasMismasPublicaciones 1: A ninguno de los dos usuarios les gusta ninguna publicación"    ~: lesGustanLasMismasPublicaciones redTigresRepCadena u2 u3 ~?= True,
    "lesGustanLasMismasPublicaciones 2: Solo a uno de los dos usuarios no le gusta ninguna publicación" ~: lesGustanLasMismasPublicaciones redIbaiNoPub u1 u5 ~?= False,
    "lesGustanLasMismasPublicaciones 3: Les gustan las mismas publicaciones a ambos usuarios"           ~: lesGustanLasMismasPublicaciones ([u6, u7], [], [pub6_1, pub7_1]) u6 u7 ~?= True,
    "lesGustanLasMismasPublicaciones 4: Los likes de un usuario están contenidos en los likes del otro usuario (pero no son iguales, y ambos le dieron like a al menos una publicación)" ~: lesGustanLasMismasPublicaciones redTigres u2 u3 ~?= False,
    "lesGustanLasMismasPublicaciones 5: NO hay likes en común entre ambos usuarios"                     ~: lesGustanLasMismasPublicaciones redTigresRepCadena u1 u3 ~?= False,
    "lesGustanLasMismasPublicaciones 6: Los dos usuarios son el mismo (u1 = u2)"                        ~: lesGustanLasMismasPublicaciones redTigres u1 u1 ~?= True
    ]
   
testEjercicio9 = test [
    "tieneUnSeguidorFiel 1: u es el único usuario que existe en la red social" ~: tieneUnSeguidorFiel red1us1pub u4 ~?= False,
    "tieneUnSeguidorFiel 2: u no tiene publicaciones" ~: tieneUnSeguidorFiel red1us0pub u4 ~?=   False,
    "tieneUnSeguidorFiel 3: (∃u2 : Usuario) (Pertenece(u2, usuarios(red)) ∧ u ̸= u2 ∧ (∀pub : Publicacion) (Pertenece(pub, publicaciones(red)) ∧ usuarioDePublicacion(pub) Pertenece(u2, likesDePublicacion(pub))= u " ~: tieneUnSeguidorFiel redIbai u5 ~?= True,
    "tieneUnSeguidorFiel 4: Existe un usuario u2 al que le gustan algunas publicaciones de u, y no le gusta ninguna otra publicación" ~: tieneUnSeguidorFiel redTigres u2 ~?= False,
    "tieneUnSeguidorFiel 5: Existe un usuario u2 al que le gustan algunas publicaciones de u, y también le gustan otras publicaciones de otros usuarios (distintos a u)" ~: tieneUnSeguidorFiel redTigresRep u2 ~?= False
    ]
   
testEjercicio10 = test [
    "existeSecuenciaDeAmigos 1: u1 = u2" ~: existeSecuenciaDeAmigos red1us0pub u4 u4 ~?= False,
    "existeSecuenciaDeAmigos 2: u1 y u2 son amigos" ~: existeSecuenciaDeAmigos redTigres u1 u2 ~?= True,
    "existeSecuenciaDeAmigos 3: u1 y u2 NO son amigos ∧ (∃us : seq⟨Usuario⟩)(CadenaDeAmigos(us, red))" ~: existeSecuenciaDeAmigos redTigres u1 u3 ~?= True,
    "existeSecuenciaDeAmigos 4: ¬(∃us : seq⟨Usuario⟩)(CadenaDeAmigos(us, red))" ~: existeSecuenciaDeAmigos redTigresRepCadena u1 u5 ~?= False
    ]


-- Corrige las comparaciones realizadas por los tests para que el orden en las n-uplas y listas no importe.
-- Extraído de test-catedra.hs
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)




-- Redes sociales y demás a ser testeadas, a modo de ejemplo:

u1    = (1, "Tomi")
u2    = (2, "Mauri")
u3    = (3, "Andy")
u4    = (4, "ForeverAlone")
u5    = (5, "Ibai")
u6    = (6, "Lucas")
u7    = (7, "Juan")
u1000 = (1000, "Roberto Carlos")

rel1_2 = (u1, u2)
rel2_3 = (u2, u3)
rel1_3 = (u1, u3)

pub1_1 = (u1, "Tres", [u2, u3])
pub1_2 = (u1, "Tristes", [u3])
pub1_3 = (u1, "Tigres", [])

pub2_1 = (u2, "Comían", [u3])
pub2_2 = (u2, "Trigo", [u3])
pub2_3 = (u2, "En", [u1])

pub3_1 = (u3, "En", [u1])
pub3_2 = (u3, "Un", [])
pub3_3 = (u3, "Trigal", [])

pub4_1 = (u4, "And Then There Were None", [])

pub5_1 = (u5, "Esta publicación solo tendrá mi propio like", [u5])
pub5_2 = (u5, "Esta publicación también solo tendrá mi propio like", [u5])
pub5_3 = (u5, "Esta publicación estará likeada por todos los de la redIbai", [u1, u2, u3, u5])


pub6_1 = (u6, "Hola", [u6, u7])
pub7_1 = (u7, "Buenos Días", [u6, u7])

redTigres = ([u1, u2, u3], [rel1_2, rel2_3], [pub1_1, pub1_2, pub1_3, pub2_1, pub2_2, pub3_1, pub3_2, pub3_3])
red1us1pub = ([u4], [], [pub4_1])
red1us0pub = ([u4], [], [])
redTigresRep = ([u1, u2, u3], [rel1_2, rel1_3, rel2_3], [pub1_1, pub2_1, pub2_2, pub3_1, pub3_2, pub3_3])
redTigresRepCadena = ([u1, u2, u3], [rel1_2, rel2_3], [pub1_1, pub1_2, pub1_3, pub2_1, pub2_2, pub2_3, pub3_1, pub3_2, pub3_3])
redIbai = ([u1, u2, u3, u5], [rel1_2, rel2_3], [pub1_1, pub1_2, pub1_3, pub2_1, pub2_2, pub3_1, pub3_2, pub3_3, pub5_1, pub5_2, pub5_3])
redIbaiNoPub = ([u1, u2, u3, u5], [rel1_2, rel2_3], [pub1_1, pub1_2, pub1_3, pub2_1, pub2_2, pub3_1, pub3_2, pub3_3])



redRoberto1 = ([u1000, u1, u2, u3, u4, u5, u6], [(u1000, u1), (u1000, u2), (u1000, u3), (u1000, u4), (u1000, u5), (u1000, u6)], [])
redRoberto2 = ([u1000, u1, u2, u3, u4], [(u1000, u1), (u1000, u2), (u1000, u3), (u1000, u4)], [])
