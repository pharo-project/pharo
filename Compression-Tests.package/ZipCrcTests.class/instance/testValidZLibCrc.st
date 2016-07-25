tests
testValidZLibCrc
	| reader writer bytes |
	writer := ZLibWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	reader := ZLibReadStream on: bytes.
	self assert: reader upToEnd = 'Hello World'.
	
	bytes := writer encodedStream contents.
	reader := ZLibReadStream on: bytes.
	self assert: (reader next: 100) = 'Hello World'.