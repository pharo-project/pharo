tests
testIndexOfWideCharacterFromTo

self assert: ('123éàôüÖẞ' indexOfWideCharacterFrom: 1 to: 9)  = 9.
self assert: ('te€st' indexOfWideCharacterFrom: 1 to: 5) = 3.
self assert: ('te€st this string' indexOfWideCharacterFrom: 4 to: 10) = 0.
self assert: ('€₤' indexOfWideCharacterFrom: 1 to: 2) = 1.