tests
testIncludesSubstring

self assert: ('testing this string' includesSubstring: 'ring'). 
self assert: ('éèàôüößäóñíá' includesSubstring: '').
self deny: ('éèàôüößäóñíá' includesSubstring: 'a'). 
self assert: ('éèàôüößäóñíá' includesSubstring: 'ßä').
self deny: ('kjdsnlksjdf' includesSubstring: 'K'). 
