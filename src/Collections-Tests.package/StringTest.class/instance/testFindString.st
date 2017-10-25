tests
testFindString

self assert: ('éèàôüößäóñíá' findString: 'ÉÈ') = 0.
self assert: ('éèàôüößäóñíá' findString: 'éè') = 1.
self assert: ('salkjsdlkgfee' findString: 'EE') = 0.
self assert: ('salkjsdlkgfee' findString: 'ee') = 12.
self assert: (' ' findString: '') = 0.