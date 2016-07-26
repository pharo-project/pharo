tests
testInvalidGZipCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes crcByte |
	writer := GZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	crcByte := bytes byteAt: bytes size-5. "before the length"
	bytes byteAt: bytes size-5 put: (crcByte + 1 bitAnd: 255).

	reader := GZipReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.
