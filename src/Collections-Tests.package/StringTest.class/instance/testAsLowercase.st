tests
testAsLowercase 

self assert: ('1 2 3 É È À Ô Ü Ö ẞ t e s T' asLowercase = '1 2 3 é è à ô ü ö ß t e s t').
self assert: ('!@#$' asLowercase = '!@#$').
self assert: ('' asLowercase = '').