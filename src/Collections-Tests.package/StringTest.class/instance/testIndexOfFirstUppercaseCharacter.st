tests
testIndexOfFirstUppercaseCharacter

self assert: ('test thiS strIng' indexOfFirstUppercaseCharacter) = 9.
self assert: ('test this string' indexOfFirstUppercaseCharacter) = 0.
self assert: ('123éàôüöß' indexOfFirstUppercaseCharacter) = 0.
self assert: ('123éàôüÖẞ' indexOfFirstUppercaseCharacter) = 8.
 