tests
testFindStringStartingAt

self assert: ('éèàôüößäóñíá' findString: 'ÉÈ' startingAt: 1) = 0.
self assert: ('éèàôüößäóñíá' findString: 'éè' startingAt: 10) = 0.
self assert: ('éèàôüößäóñíá' findString: 'ß' startingAt: 5) = 7.
self assert: ('salkjsdlkgfee' findString: 'lke' startingAt: 1) = 0.
self assert: ('salkjsdlkgfee' findString: 'ee'startingAt: 3) = 12.
self assert: (' ' findString: '' startingAt: 1) = 0.