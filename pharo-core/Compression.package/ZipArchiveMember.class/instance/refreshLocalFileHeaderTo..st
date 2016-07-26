private-writing
refreshLocalFileHeaderTo: aStream
	"Re-writes my local header to the given stream.
	To be called after writing the data stream.
	Assumes that fileName and localExtraField sizes didn't change since last written."

	| here systemFileName |
	self flag: 'WTF! What is the purpose of this method?! We just wrote the headers. Why write them again?'.
	here := aStream position.
	systemFileName := fileName asVmPathName.
	aStream position: writeLocalHeaderRelativeOffset.

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

	aStream position: here.
