tests
testIndexOf

self assert: ('test this string' indexOf: $k) = 0.
self assert: ('test this string' indexOf: 't') = 0.
self assert: ('test this string' indexOf: $t) = 1.
self assert: ('123éàôüöß' indexOf: $ß) = 9.
self assert: ('123éàôüöß' indexOf: $E) = 0.
 