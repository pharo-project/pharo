tests
testFindStringStartingAtCaseSensitive

self assert: ('éèàôüößäóñíá' findString: 'Á' startingAt: 1 caseSensitive: false) = 12.
self assert: ('éèàôüößäóñíá' findString: 'Á' startingAt: 1 caseSensitive: true) = 0.
self assert: ('suiehl5tlr' findString: 'r' startingAt:  5 caseSensitive: true) = 10.
self assert: ('test' findString: 'T' startingAt: 3 caseSensitive: false) = 4.
self assert: ('test' findString: '' startingAt: 1 caseSensitive: false) = 0.