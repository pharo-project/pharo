tests
testIncludesSubstringCaseSensitive

self deny: ('test this string' includesSubstring: 'Ring' caseSensitive: true ). 
self assert: ('test this string' includesSubstring: 'Ring' caseSensitive: false). 
self deny: ('123éàôüöß' includesSubstring: '' caseSensitive: false). 
self assert: ('123éàôüöß' includesSubstring: 'öß' caseSensitive: true).
self assert: ('123éàôüöß' includesSubstring: 'ÀÔ' caseSensitive: false).

