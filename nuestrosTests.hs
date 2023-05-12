import Test.HUnit
import Solucion

-- Casos de Test

main = run1 ++ run2 ++ run3 ++ run4 ++ run5 ++ run6 ++ run7 ++ run8 ++ run9 ++ run10

run1 = runTestTT testEjercicio1
run2 = runTestTT testEjercicio2
run3 = runTestTT testEjercicio3
run4 = runTestTT testEjercicio4
run5 = runTestTT testEjercicio5
run6 = runTestTT testEjercicio6
run7 = runTestTT testEjercicio7
run8 = runTestTT testEjercicio8
run9 = runTestTT testEjercicio9
run10 = runTestTT testEjercicio10


-- Nota temporal, estos casos fueron sacados de la clase práctica a modo de ejemplo
-- y para visualizar la sintaxis y formato que deben llevar los tests, no son del TP
-- Esquema de los test suites: https://docs.google.com/spreadsheets/d/1ISs0Z0TMPZ_7Ro_69iO2XR96wovZvKPW/edit#gid=1370724871

testEjercicio1 = test [
    "nombresDeUsuarios 1: Lista de usuarios vacía"         ~: ([],[],[])                 ~?= [],
    "nombresDeUsuarios 2: Lista de usuarios con repetidos" ~: (["u2", "u3", "u2"],[],[]) ~?= ["Mauri", "Andy"],
    "nombresDeUsuarios 3: Lista de usuarios sin repetidos" ~: redA                       ~?= ["Tomi", "Mauri", "Andy"],
    ]

testEjercicio2 = test [
    "amigosDe 1: u no tiene amigos" ~: redB u4 ~?= [],
    "amigosDe 2: u tiene amigos CON nombres repetidos ∧ IDs distintos" ~: ([(1, "Tomi"), (2, "u"), (727, "Tomi")],[(2, u), (1,"Tomi")],[(2, u), (727,"Tomi")], []) ~?= [(1, "Tomi"),(727, "Tomi")],
    "amigosDe 3: u tiene amigos, pero SIN nombres repetidos" ~: redA u3 ~?= [2, "Mauri"],
    ]
    
testEjercicio3 = test [
    "cantidadDeAmigos 1: u no tiene amigos :("                                                             ~: redB u4 ~?= 0,
    "cantidadDeAmigos 2: u tiene un solo amigo"                                                            ~: redA u1 ~?= 1,
    "cantidadDeAmigos 3: u tiene más (mayor estricto) de un amigo (tiene una cantidad finita n de amigos)" ~: redA u2 ~?= 2,
]

testEjercicio4 = test [
    "usuarioConMasAmigos 1: La red tiene un solo usuario y ninguna relación." ~: redB     ~?= u4,
    "usuarioConMasAmigos 2: Dos usuarios tienen la misma cantidad de amigos"  ~: redC     ~?= u1 || u2 || u3,
    "usuarioConMasAmigos 3: Un usuario tiene una cantidad de amigos mayor estricta a la cantidad de amigos de los demás usuarios" ~: redA ~?= u2,
]
   
testEjercicio5 = test [
    "estaRobertoCarlos 1: cantidadDeAmigos > 1000000" ~:     ~?= True,
    "estaRobertoCarlos 2: cantidadDeAmigos = 1000000" ~:     ~?= False,
    "estaRobertoCarlos 3: cantidadDeAmigos < 1000000" ~:     ~?= False,
]
   
testEjercicio6 = test [
    "publicacionesDe 1: u no tiene publicaciones"                        ~: redB u4 ~?= [],
    "publicacionesDe 2: u tiene una sola publicación"                    ~: redC u1 ~?= [pub1_1],
    "publicacionesDe 3: u tiene más (mayor estricto) de una publicación" ~: redA u3 ~?= [pub3_1, pub3_2, pub3_3],
]
   
testEjercicio7 = test [
    "publicacionesQueLeGustanA 1: A u no le gusta ninguna publicación" ~:     ~?= [],
    "publicacionesQueLeGustanA 2: (∀n>=2) (Pertenece u, pub_n[1]) ∧ (pub_n[1] = pub_n-1[1])" ~:        ~?=   ,                             -- Pendiente: formalizar la descripción de este caso (¿o alcanza con este grado de formalidad?)
    "publicacionesQueLeGustanA 3: Hay publicaciones con un mismo autor que le gustan a u" ~:      ~?=     ,
   -- DEPRECATED: "publicacionesQueLeGustanA 4: No hay publicaciones repetidas entre las publicaciones que le gustan a u" ~:      ~?=     ,
]
   
testEjercicio8 = test [
    "lesGustanLasMismasPublicaciones 1: A ninguno de los dos usuarios les gusta ninguna publicación" ~:     ~?= True,
    "lesGustanLasMismasPublicaciones 2: Solo a uno de los dos usuarios no le gusta ninguna publicación" ~:        ~?= False,
    "lesGustanLasMismasPublicaciones 3: Les gustan las mismas publicaciones a ambos usuarios" ~:      ~?= True,
    "lesGustanLasMismasPublicaciones 4: Hay algunos (pero no todos) likes en comùn entre ambos usuarios" ~:      ~?= False,
    "lesGustanLasMismasPublicaciones 5: Los likes de un usuario están contenidos en los likes del otro usuario (pero no son iguales)" ~:      ~?= False,
    "lesGustanLasMismasPublicaciones 6: NO hay likes en comùn entre ambos usuarios" ~:       ~?= False,
    "lesGustanLasMismasPublicaciones 7: Los dos usuarios son el mismo (u1 = u2)" ~:        ~?= True,
]
   
testEjercicio9 = test [
    "tieneUnSeguidorFiel 1: u es el único usuario que existe en la red social" ~:     ~?= False,
    "tieneUnSeguidorFiel 2: u no tiene publicaciones" ~:        ~?=   False,
    "tieneUnSeguidorFiel 3: (∃u2 : Usuario) (Pertenece(u2, usuarios(red)) ∧ u ̸= u2 ∧ (∀pub : Publicacion) (Pertenece(pub, publicaciones(red)) ∧ usuarioDePublicacion(pub) Pertenece(u2, likesDePublicacion(pub))= u " ~:      ~?= True,
    -- DEPRECATED: "tieneUnSeguidorFiel 4: (∃u2 : Usuario) (Pertenece(u2, usuarios(red)) ∧ u ̸= u2 ∧ (∀pub : Publicacion) (Pertenece(pub, publicaciones(red)) ∧ usuarioDePublicacion(pub) ¬Pertenece(u2, likesDePublicacion(pub))= u " ~:      ~?= False,
    "tieneUnSeguidorFiel 4: Existe un usuario u2 al que le gustan algunas publicaciones de u, y no le gusta ninguna otra publicación" ~:        ~?=   False,
    "tieneUnSeguidorFiel 5: Existe un usuario u2 al que le gustan algunas publicaciones de u, y tambièn le gustan otras publicaciones de otros usuarios (distintos a u y u2)" ~:        ~?=   False,
]
   
testEjercicio10 = test [
    "existeSecuenciaDeAmigos 1: u1 = u2" ~:     ~?= False,
    "existeSecuenciaDeAmigos 2: u1 y u2 son amigos" ~:        ~?= True,
    "existeSecuenciaDeAmigos 3: u1 y u2 NO son amigos ∧ (∃us : seq⟨Usuario⟩)(CadenaDeAmigos(us, red))" ~:      ~?= True,
    "existeSecuenciaDeAmigos 4: ¬(∃us : seq⟨Usuario⟩)(CadenaDeAmigos(us, red))" ~:       ~?= False,
]


-- Corrige las comparaciones realizadas por los tests para que el orden en las n-uplas y listas no importe.
-- Copypasteado de test-catedra.hs
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)




-- Redes sociales y demás, a modo de ejemplo:

u1 = (1, "Tomi")
u2 = (2, "Mauri")
u3 = (3, "Andy")
u4 = (4, "ForeverAlone")

rel1_2 = (u1, u2)
rel2_3 = (u2, u3)

pub1_1 = (u1, "Tres", [u2, u3])
pub1_2 = (u1, "Tristes", [u3])
pub1_3 = (u1, "Tigres", [])

pub2_1 = (u2, "Comían", [u1])
pub2_2 = (u2, "Trigo", [u1, u3])

pub3_1 = (u3, "En", [])
pub3_2 = (u3, "Un", [])
pub3_3 = (u3, "Trigal", [])




redA = ([Tomi, Mauri, Andy], [rel1_2, rel2_3], [pub1_1, pub1_2, pub1_3, pub2_1, pub2_2, pub3_1, pub3_2, pub3_3)
redB = ([ForeverAlone], [], [])
redC = ([Tomi, Mauri, Andy], [rel1_2, rel1_3, rel2_3], [pub1_1, pub2_1, pub2_2, pub3_1, pub3_2, pub3_3)
