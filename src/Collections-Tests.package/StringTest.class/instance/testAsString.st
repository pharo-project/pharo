tests
testAsString 

self assert: (123 asString) = '123'.
self assert: (#(1 2 3) asString) = '#(1 2 3)'.
self assert: ((OrderedCollection new) asString) = 'an OrderedCollection()'.
self assert: ((1 < 3) asString) = 'true'.
self assert: ('é è à ô ü ö ß ä ó ñ í á' asString) = 'é è à ô ü ö ß ä ó ñ í á'.
self assert: ('' asString) = ''.