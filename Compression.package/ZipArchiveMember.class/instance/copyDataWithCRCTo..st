private-writing
copyDataWithCRCTo: aStream
	"Copy my data to aStream. Also set the CRC-32.
	Only used when compressionMethod = desiredCompressionMethod = CompressionStored"

	uncompressedSize := compressedSize := readDataRemaining.

	crc32 := 16rFFFFFFFF.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		aStream nextPutAll: data.
		crc32 := ZipWriteStream updateCrc: crc32 from: 1 to: data size in: data.
		readDataRemaining := readDataRemaining - data size.
	].

	crc32 := crc32 bitXor: 16rFFFFFFFF.
