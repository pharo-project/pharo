tests
testInvalidZLibCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes crcByte |
	writer := ZLibWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	crcByte := bytes byteAt: bytes size-2.
	bytes byteAt: bytes size-2 put: (crcByte + 1 bitAnd: 255).

	reader := ZLibReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.
