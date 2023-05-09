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
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
    ]

testEjercicio2 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
    ]
    
testEjercicio3 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
    ]

testEjercicio4 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
    ]
   
   testEjercicio5 = test [
    "Caso 1: Multiplos de 0" ~:     ~?=   ,
    "Caso 2: Lista vacia" ~:        ~?=   ,
    "Caso 3: Hay un solo multiplo" ~:      ~?=     ,
    "Caso 4: No hay multiplos con n neg" ~:       ~?=    ,
    "Caso 5: Hay mas de un multiplo con n neg" ~:        ~?=     ,
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
