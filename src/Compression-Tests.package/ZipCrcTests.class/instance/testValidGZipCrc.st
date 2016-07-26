tests
testValidGZipCrc
	| reader writer bytes |
	writer := GZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	reader := GZipReadStream on: bytes.
	self assert: reader upToEnd = 'Hello World'.