tests
testOnlyLetters

self assert: ('abc98def' onlyLetters) = 'abcdef'.
self assert: ('abc 98 12 def' onlyLetters) = 'abcdef'.
self assert: ('012éà ôüÖ ẞ345' onlyLetters) = 'éàôüÖẞ'.
self assert: ('123 456' onlyLetters) = ''.
self assert: ('' onlyLetters) = ''.

