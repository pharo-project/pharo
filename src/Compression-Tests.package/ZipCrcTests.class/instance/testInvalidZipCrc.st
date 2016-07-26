tests
testInvalidZipCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes |
	writer := ZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader contents] raise: CRCError.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader next: 100] raise: CRCError.
