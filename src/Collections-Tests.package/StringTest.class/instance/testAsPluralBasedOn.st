tests
testAsPluralBasedOn

self assert: ('test' asPluralBasedOn: #(1)) = 'test'.
self assert: ('test' asPluralBasedOn: #(1 2 3)) = 'tests'.
self assert: ('test' asPluralBasedOn: 1) = 'test'.
self assert: ('test' asPluralBasedOn: false) = 'tests'.
self assert: ('é è à ô ü ö ß ä ó ñ í á' asPluralBasedOn: 10) = 'é è à ô ü ö ß ä ó ñ í ás'.
self assert: ('' asPluralBasedOn: 1) = ''.
self assert: ('%' asPluralBasedOn: 10) = '%s'.