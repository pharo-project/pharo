tests
testCaseSensitiveLessOrEqual

self assert: ('EEA' caseSensitiveLessOrEqual: 'fgh').
self deny: ('éèàôüößäóñíá' caseSensitiveLessOrEqual: 'éèàôüöß'). 
self assert: ('éèàôüößäóñíá' caseSensitiveLessOrEqual: 'éèàôüößäóñíá').
self assert: ('ÉÈÀÔÜÖ' caseSensitiveLessOrEqual: 'èàôüö').
self assert: ('eea' caseSensitiveLessOrEqual: 'ÉÈÀ').