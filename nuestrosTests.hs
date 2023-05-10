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
    "Caso 1: Lista de usuarios vacía" ~: nombresDeUsuarios 1                ~?= [],
    "Caso 2: Lista de usuarios con repetidos" ~: nombresDeUsuarios 2        ~?=   ,
    "Caso 3: Lista de usuarios sin repetidos" ~: nombresDeUsuarios 3        ~?=   ,
    ]

testEjercicio2 = test [
    "Caso 1: u no tiene amigos" ~:     ~?= [],
    "Caso 2: u no tiene amigos con nombres repetidos" ~:      ~?=     ,
    "Caso 3: u no tiene amigos con nombres repetidos" ~:       ~?=    ,
    ]
    
testEjercicio3 = test [
    "Caso 1: u no tiene amigos :(" ~:     ~?= 0,
    "Caso 2: u tiene un solo amigo" ~:        ~?= 1,
    "Caso 3: u tiene más (mayor estricto) de un amigo (tiene una cantidad finita n de amigos)" ~:      ~?= ,
]

testEjercicio4 = test [
    "Caso 1: La red tiene un solo usuario y ninguna relación." ~:     ~?=   ,
    "Caso 2: La red tiene un único usuario u y una única relación: (u, u)" ~:        ~?=   ,
    "Caso 3: Dos usuarios tienen la misma cantidad de amigos" ~:      ~?=     ,
    "Caso 4: Un usuario tiene una cantidad de amigos mayor estricta a la cantidad de amigos de los demás usuarios" ~:       ~?=    ,
]
   
testEjercicio5 = test [
    "Caso 1: cantidadDeAmigos > 1000000" ~:     ~?= True,
    "Caso 2: cantidadDeAmigos = 1000000" ~:     ~?= False,
    "Caso 3: cantidadDeAmigos < 1000000" ~:     ~?= False,
]
   
testEjercicio6 = test [
    "Caso 1: u no tiene publicaciones" ~:     ~?= [],
    "Caso 2: u tiene una sola publicación" ~:        ~?=   ,
    "Caso 3: u tiene más (mayor estricto) de una publicación" ~:      ~?=     ,
]
   
testEjercicio7 = test [
    "Caso 1: A u no le gusta ninguna publicación" ~:     ~?= [],
    "Caso 2: A u le gustan publicaciones repetidas (dos o más usuarios con una misma publicación)" ~:        ~?=   ,       -- Pendiente: formalizar la descripción de este caso
    "Caso 3: No hay publicaciones repetidas entre las publicaciones que le gustan a u" ~:      ~?=     ,
]
   
testEjercicio8 = test [
    "Caso 1: A ninguno de los dos usuarios les gusta ninguna publicación" ~:     ~?=   ,
    "Caso 2: Solo a uno de los dos usuarios no le gusta ninguna publicación" ~:        ~?=   ,
    "Caso 3: Les gustan las mismas publicaciones a ambos usuarios" ~:      ~?= True,
    "Caso 4: NO les gustan las mismas publicaciones a ambos usuarios" ~:       ~?= False,
    "Caso 5: Los dos usuarios son el mismo (u1 = u2)" ~:        ~?= True,
]
   
testEjercicio9 = test [
    "Caso 1: u es el único usuario que existe en la red social" ~:     ~?= True,
    "Caso 2: u no tiene publicaciones" ~:        ~?=   False,
    "Caso 3: (∃u2 : Usuario)(Pertenece(u2, usuarios(red)) ∧ u ̸= u2 ∧ (∀pub : Publicacion)(Pertenece(pub, publicaciones(red)) ∧ usuarioDePublicacion(pub) Pertenece(u2, likesDePublicacion(pub))= u " ~:      ~?= True,
    "Caso 4: (∃u2 : Usuario)(Pertenece(u2, usuarios(red)) ∧ u ̸= u2 ∧ (∀pub : Publicacion)(Pertenece(pub, publicaciones(red)) ∧ usuarioDePublicacion(pub) ¬Pertenece(u2, likesDePublicacion(pub))= u " ~:      ~?= False,
]
   
testEjercicio10 = test [
    "Caso 1: u1 = u2" ~:     ~?=   ,
    "Caso 2: u1 y u2 son amigos" ~:        ~?= True,
    "Caso 3: u1 y u2 NO son amigos ∧ (∃us : seq⟨Usuario⟩)(CadenaDeAmigos(us, red))" ~:      ~?= True,
    "Caso 4: ¬(∃us : seq⟨Usuario⟩)(CadenaDeAmigos(us, red))" ~:       ~?= False,
]


-- Corrige las comparaciones realizadas por los tests para que el orden en las n-uplas y listas no importe.
-- Copypasteado de test-catedra.hs
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
