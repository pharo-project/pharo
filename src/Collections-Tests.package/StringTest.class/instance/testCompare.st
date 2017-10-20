tests
testCompare

self assert: ('EEA' compare: 'fgh') = 1.
self assert: ('ABCDEFGH' compare: 'ABCDEFGH') = 2.
self assert: ('fgh' compare: 'fg') = 3.
self assert: ('éèàôüöß' compare: 'ÉÈÀÔÜÖẞ') = 1.
self assert: ('éèàôüößäóñíá' compare: 'éèàôüößäóñíá') = 2.
self assert: ('ÉÈÀÔÜ' compare: 'ÔÜÖ') = 1.
self assert: ('eeao' compare: 'éèàô') = 1.
self assert: ('!@#' compare: '123') = 1.