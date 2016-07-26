writing
writeIHDRChunkOn: aStream
	"Write the IHDR chunk"
	aStream nextPutAll: 'IHDR' asByteArray.
	aStream nextInt32Put: width.
	aStream nextInt32Put: height.
	aStream nextNumber: 1 put: bitsPerChannel.
	aStream nextNumber: 1 put: colorType.
	aStream nextNumber: 1 put: 0. "compression"
	aStream nextNumber: 1 put: 0. "filter method"
	aStream nextNumber: 1 put: 0. "interlace method"
