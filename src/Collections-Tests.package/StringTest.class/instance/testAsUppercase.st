tests
testAsUppercase 

self assert: ('éèàôütest' asUppercase) = 'ÉÈÀÔÜTEST'.
self assert: ('123éàôüößTEST' asUppercase) = '123ÉÀÔÜÖẞTEST'.
self assert: ('' asUppercase) = ''.