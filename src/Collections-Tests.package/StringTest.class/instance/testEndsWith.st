tests
testEndsWith 

self deny: ('éèàôüößäóñíá' endsWith: 'ia').
self deny: ('éèàôüößäóñíá' endsWith: '').
self assert: ('éèàôüößäóñíá' endsWith: 'éèàôüößäóñíá').
self deny: ('test' endsWith: 'ST').
self assert: ('test' endsWith: 'st').
self deny: ('ÉÈÀÔÜÖẞÄÓÑÍÁ' endsWith: 'íá').
self deny: ('test' endsWith: 'tester').
