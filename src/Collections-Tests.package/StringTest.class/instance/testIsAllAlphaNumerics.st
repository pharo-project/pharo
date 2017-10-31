tests
testIsAllAlphaNumerics

self assert: ('123dwef890' isAllAlphaNumerics).
self deny: ('12 34 top &^' isAllAlphaNumerics). 
self assert: ('' isAllAlphaNumerics).
self deny: (' 234 ' isAllAlphaNumerics).
self deny: ('()' isAllAlphaNumerics). 
self assert: ('123éàôüÖẞ' isAllAlphaNumerics).