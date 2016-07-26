private-writing
writeLocalFileHeaderTo: aStream
	"Write my local header to a file handle.
	Stores the offset to the start of the header in my
	writeLocalHeaderRelativeOffset member."

	| systemFileName |
	systemFileName := fileName asVmPathName.
	aStream nextPutAll: LocalFileHeaderSignature.
	aStream nextLittleEndianNumber: 2 put: versionNeededToExtract.
	aStream nextLittleEndianNumber: 2 put: self bitFlag.
	aStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.

	aStream nextLittleEndianNumber: 4 put: lastModFileDateTime asDosTimestamp.
	aStream nextLittleEndianNumber: 4 put: crc32.
	aStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).
	aStream nextLittleEndianNumber: 4 put: uncompressedSize.

	aStream nextLittleEndianNumber: 2 put: systemFileName size.
	aStream nextLittleEndianNumber: 2 put: localExtraField size.

	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: localExtraField asByteArray.
