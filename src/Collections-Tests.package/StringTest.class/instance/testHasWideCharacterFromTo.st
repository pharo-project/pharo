tests
testHasWideCharacterFromTo

self assert: ('te€st' hasWideCharacterFrom: 1 to: 3).
self deny: ('test' hasWideCharacterFrom: 1 to: 4).  
self assert: ('€₤' hasWideCharacterFrom: 1 to: 2).  
self assert: ('test this string €₤' hasWideCharacterFrom: 18 to: 19).