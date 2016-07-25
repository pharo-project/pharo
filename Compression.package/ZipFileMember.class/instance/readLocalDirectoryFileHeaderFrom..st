private-reading
readLocalDirectoryFileHeaderFrom: aStream 
	"Positions stream as necessary. Will return stream to its original position"

	| fileNameLength extraFieldLength xcrc32 xcompressedSize xuncompressedSize sig oldPos |

	oldPos := aStream position.

	aStream position: localHeaderRelativeOffset.

	sig := aStream next: 4.
	sig = LocalFileHeaderSignature asByteArray
		ifFalse: [ aStream position: oldPos.
				^self error: 'bad LH signature at ', localHeaderRelativeOffset printStringHex ].

	versionNeededToExtract := aStream nextLittleEndianNumber: 2.
	self bitFlag: (aStream nextLittleEndianNumber: 2).
	compressionMethod := aStream nextLittleEndianNumber: 2.
	lastModFileDateTime := self unzipTimestamp: (aStream nextLittleEndianNumber: 4).
	xcrc32 := aStream nextLittleEndianNumber: 4.
	xcompressedSize := aStream nextLittleEndianNumber: 4.
	xuncompressedSize := aStream nextLittleEndianNumber: 4.

	fileNameLength := aStream nextLittleEndianNumber: 2.
	extraFieldLength := aStream nextLittleEndianNumber: 2.

	fileName := (aStream next: fileNameLength) asString.
	localExtraField := (aStream next: extraFieldLength) asByteArray.

	dataOffset := aStream position.

	"Don't trash these fields if we already got them from the central directory"
	self hasDataDescriptor ifFalse: [
		crc32 := xcrc32.
		compressedSize := xcompressedSize.
		uncompressedSize := xuncompressedSize.
	].

	aStream position: oldPos.