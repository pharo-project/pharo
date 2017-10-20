tests
testIsWideString

self assert: ('€₤' isWideString).
self deny: ('test this string' isWideString). 
self assert: (' test ₤' isWideString).
self deny: ('' isWideString). 
self assert: ('éàôüÖẞ' isWideString).
self deny: ('éàôüÖ' isWideString). 