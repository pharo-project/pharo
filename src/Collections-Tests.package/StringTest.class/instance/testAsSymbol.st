tests
testAsSymbol

self assert: ('hel','lo' asSymbol) = #'hello'.
self assert: ('é è à ô ü' asSymbol)  = #'é è à ô ü'.
self assert: ('' asSymbol) = #''.
self assert: ('test' asSymbol) == #'test'. 