tests
testOccursInWithEmptyCaseSensitive

self assert: ('abc98def' occursInWithEmpty: 'abc' caseSensitive: true). 
self deny: ('abc98def' occursInWithEmpty: 'Abc' caseSensitive: true). 
self assert: ('abc98def' occursInWithEmpty: 'Abc' caseSensitive: false).
self assert: ('012éà ôüÖ ẞ345' occursInWithEmpty: '012' caseSensitive: true).
self deny: ('012éàôüÖ ẞ345' occursInWithEmpty: 'ÉÀÔÜÖ' caseSensitive: true).
