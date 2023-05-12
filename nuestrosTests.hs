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
    "nombresDeUsuarios 1: Lista de usuarios vacía" ~:              ~?= [],
    "nombresDeUsuarios 2: Lista de usuarios con repetidos" ~:         ~?=   ,
    "nombresDeUsuarios 3: Lista de usuarios sin repetidos" ~:         ~?=   ,
    ]

testEjercicio2 = test [
    "amigosDe 1: u no tiene amigos" ~:     ~?= [],
    "amigosDe 2: u tiene amigos con nombres repetidos ∧ IDs distintos" ~:      ~?=     ,
    "amigosDe 3: u no tiene amigos con nombres repetidos" ~:       ~?=    ,
    ]
    
testEjercicio3 = test [
    "cantidadDeAmigos 1: u no tiene amigos :(" ~:     ~?= 0,
    "cantidadDeAmigos 2: u tiene un solo amigo" ~:        ~?= 1,
    "cantidadDeAmigos 3: u tiene más (mayor estricto) de un amigo (tiene una cantidad finita n de amigos)" ~:      ~?= ,
]

testEjercicio4 = test [
    "usuarioConMasAmigos 1: La red tiene un solo usuario y ninguna relación." ~:     ~?=   ,
    "usuarioConMasAmigos 2: La red tiene un único usuario u y una única relación: (u, u)" ~:        ~?=   ,
    "usuarioConMasAmigos 3: Dos usuarios tienen la misma cantidad de amigos" ~:      ~?=     ,
    "usuarioConMasAmigos 4: Un usuario tiene una cantidad de amigos mayor estricta a la cantidad de amigos de los demás usuarios" ~:       ~?=    ,
]
   
testEjercicio5 = test [
    "estaRobertoCarlos 1: cantidadDeAmigos > 1000000" ~:     ~?= True,
    "estaRobertoCarlos 2: cantidadDeAmigos = 1000000" ~:     ~?= False,
    "estaRobertoCarlos 3: cantidadDeAmigos < 1000000" ~:     ~?= False,
]
   
testEjercicio6 = test [
    "publicacionesDe 1: u no tiene publicaciones" ~:     ~?= [],
    "publicacionesDe 2: u tiene una sola publicación" ~:        ~?=   ,
    "publicacionesDe 3: u tiene más (mayor estricto) de una publicación" ~:      ~?=     ,
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
    "lesGustanLasMismasPublicaciones 4: Los likes de un usuario están contenidos en los likes del otro usuario (pero no son iguales)" ~:      ~?= False,
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
