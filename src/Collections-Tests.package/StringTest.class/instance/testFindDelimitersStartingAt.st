tests
testFindDelimitersStartingAt

self assert: ('this is a string' findDelimiters: ' ' startingAt: 1) = 5.
self assert: ('this is a string' findDelimiters: 'i' startingAt: 1) = 3.
self assert: ('this is a string' findDelimiters: #($a $e $s) startingAt: 5) = 7.
self assert: ('éèàôüöß éèàôüöß' findDelimiters: 'ß' startingAt: 2) = 7.