tests
testMissingZipCrc
	"See that the lack of a CRC does not raise an error"

	| reader writer bytes readBytes |
	writer := ZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.
	bytes := writer encodedStream contents.
	reader := ZipReadStream on: bytes.
	readBytes := reader upToEnd.
	self assert: readBytes = 'Hello World'.
	reader := ZipReadStream on: bytes.
	reader contents.
	reader := ZipReadStream on: bytes.
	reader next: 100