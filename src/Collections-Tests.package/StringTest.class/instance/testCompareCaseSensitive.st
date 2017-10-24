tests
testCompareCaseSensitive

self assert: ('EEA' compare: 'fgh' caseSensitive: true) = 1.
self assert: ('ABCDEFGH' compare: 'abcdefgh' caseSensitive: false) = 2.
self assert: ('fgh' compare: 'fg' caseSensitive: false) = 3.
self assert: ('éèàôüößäóñíá' compare: 'éèàôüößäóñíá' caseSensitive: false) = 2.
self assert: ('ÉÈÀÔÜ' compare: 'ÔÜÖ' caseSensitive: true) = 1.
self assert: ('éèàôüöß' compare: 'ÉÈÀÔÜÖẞ' caseSensitive: false) = 1.