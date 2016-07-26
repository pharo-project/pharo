testing
testAsFileReference
	| fileUrl fileReference |
	fileUrl := 'file:///foo/bar/test.txt' asZnUrl.
	fileReference := fileUrl asFileReference.
	self assert: fileReference basename equals: 'test.txt'.
	self assert: fileReference pathSegments asArray equals: #( 'foo' 'bar' 'test.txt' ).
	self assert: fileReference isAbsolute 