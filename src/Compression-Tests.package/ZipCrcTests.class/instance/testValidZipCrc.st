tests
testValidZipCrc
	"See that a correct CRC does not raise an error and that we can read what we wrote."

	| reader writer bytes readBytes |
	writer := ZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.
	bytes := writer encodedStream contents.
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	readBytes := reader upToEnd.
	self assert: readBytes = 'Hello World'.
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	readBytes := reader contents.
	self assert: readBytes = 'Hello World'.
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	readBytes := reader next: 11.
	self assert: readBytes = 'Hello World'.
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	readBytes := reader next: 100.
	self assert: readBytes = 'Hello World'