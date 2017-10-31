tests
testInitialIntegerOrNil

self assert: ('234Whoopie' initialIntegerOrNil) = 234.
self assert: ('wimpy' initialIntegerOrNil) isNil.
self assert: ('234' initialIntegerOrNil) = 234.
self assert: ('2N' initialIntegerOrNil) = 2.
self assert: ('  89Ten ' initialIntegerOrNil) isNil.
self assert: ('78 92' initialIntegerOrNil) = 78.
self assert: ('123éàôüÖẞ' initialIntegerOrNil) = 123.