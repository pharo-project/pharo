private-writing
writeDataDescriptorTo: aStream
	"This writes a data descriptor to the given stream.
	Assumes that crc32, writeOffset, and uncompressedSize are
	set correctly (they should be after a write).
	Further, the local file header should have the
	GPBF:=HAS:=DATA:=DESCRIPTOR:=MASK (8) bit set."

	aStream nextLittleEndianNumber: 4 put: crc32.
	aStream nextLittleEndianNumber: 4 put: compressedSize.
	aStream nextLittleEndianNumber: 4 put: uncompressedSize.