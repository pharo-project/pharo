tests
testAsSignedInteger

self assert: ('test' asSignedInteger) = nil.
self assert: ('é è à ô ü ö ß ä ó ñ í á' asSignedInteger ) = nil.
self assert: ('test-10test' asSignedInteger) = -10.
self assert: ('' asSignedInteger) = nil.
self assert: ('1234567890-1234567890' asSignedInteger) = 1234567890.
self assert: ('--12345' asSignedInteger) = -12345.