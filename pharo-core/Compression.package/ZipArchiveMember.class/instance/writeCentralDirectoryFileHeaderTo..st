private-writing
writeCentralDirectoryFileHeaderTo: aStream
	"C2 v3 V4 v5 V2"

	| systemFileName systemFileComment systemCdExtraField |
	systemFileName := fileName asVmPathName.
	systemFileComment := fileComment convertToSystemString.
	systemCdExtraField := cdExtraField.
	aStream nextPutAll: CentralDirectoryFileHeaderSignature.
	aStream nextLittleEndianNumber: 1 put: self versionMadeBy.
	aStream nextLittleEndianNumber: 1 put: fileAttributeFormat.

	aStream nextLittleEndianNumber: 2 put: versionNeededToExtract.
	aStream nextLittleEndianNumber: 2 put: self bitFlag.
	aStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.

	aStream nextLittleEndianNumber: 4 put: lastModFileDateTime asDosTimestamp.

	"These next 3 should have been updated during the write of the data"
	aStream nextLittleEndianNumber: 4 put: crc32.
	aStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).
	aStream nextLittleEndianNumber: 4 put: uncompressedSize.

	aStream nextLittleEndianNumber: 2 put: systemFileName size.
	aStream nextLittleEndianNumber: 2 put: systemCdExtraField size.
	aStream nextLittleEndianNumber: 2 put: systemFileComment size.
	aStream nextLittleEndianNumber: 2 put: 0.		"diskNumberStart"
	aStream nextLittleEndianNumber: 2 put: internalFileAttributes.

	aStream nextLittleEndianNumber: 4 put: externalFileAttributes.
	aStream nextLittleEndianNumber: 4 put: writeLocalHeaderRelativeOffset.

	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: systemCdExtraField asByteArray.
	aStream nextPutAll: systemFileComment asByteArray.