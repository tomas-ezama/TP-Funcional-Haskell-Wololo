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

testEjercicio1 = test [
    "Caso 1: Lista de usuarios vacía" ~: nombresDeUsuarios ()                ~?= [],
    "Caso 2: Lista de usuarios con repetidos" ~: nombresDeUsuarios ()        ~?=   ,
    "Caso 3: Lista de usuarios sin repetidos" ~: nombresDeUsuarios ()        ~?=   ,
    ]

testEjercicio2 = test [
    "Caso 1: u no tiene amigos" ~:     ~?=   ,
    "Caso 2: u es amigo de sí mismo" ~:        ~?=   ,
    "Caso 3: u tiene amigos repetidos" ~:      ~?=     ,
    "Caso 4: u no tiene amigos repetidos" ~:       ~?=    ,
    ]
    
testEjercicio3 = test [
    "Caso 1: u no tiene amigos :(" ~:     ~?=   ,
    "Caso 2: u tiene un solo amigo" ~:        ~?=   ,
    "Caso 3: u tiene más de un amigo (tiene una cantidad finita n de amigos)" ~:      ~?=     ,
]

testEjercicio4 = test [
    "Caso 1: La red tiene un solo usuario y ninguna relación." ~:     ~?=   ,
    "Caso 2: La red tiene un único usuario u y una única relación: (u, u)" ~:        ~?=   ,
    "Caso 3: Dos usuarios tienen la misma cantidad de amigos" ~:      ~?=     ,
    "Caso 4: Usuario con mayor estricto cantidad de amigos que el resto" ~:       ~?=    ,
]
   
testEjercicio5 = test [
    "Caso 1: cantidadDeAmigos > 1000000" ~:     ~?= True,
    "Caso 2: cantidadDeAmigos = 1000000" ~:     ~?= False,
    "Caso 3: cantidadDeAmigos < 1000000" ~:     ~?= False,
]
   
testEjercicio6 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
]
   
testEjercicio7 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
]
   
testEjercicio8 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
]
   
testEjercicio9 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
]
   
testEjercicio10 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
]


-- Corrige las comparaciones realizadas por los tests para que el orden en las n-uplas y listas no importe.
-- Copypasteado de test-catedra.hs
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
