private
readEndOfCentralDirectoryFrom: aStream
	"Read EOCD, starting from position before signature."
	| signature zipFileCommentLength |
	signature := self readSignatureFrom: aStream.
	signature = EndOfCentralDirectorySignature ifFalse: [ ^self error: 'bad signature at ', aStream position printString ].

	aStream nextLittleEndianNumber: 2. "# of this disk"
	aStream nextLittleEndianNumber: 2. "# of disk with central dir start"
	aStream nextLittleEndianNumber: 2. "# of entries in central dir on this disk"
	aStream nextLittleEndianNumber: 2. "total # of entries in central dir"
	centralDirectorySize := aStream nextLittleEndianNumber: 4. "size of central directory"
	centralDirectoryOffsetWRTStartingDiskNumber := aStream nextLittleEndianNumber: 4. "offset of start of central directory"
	zipFileCommentLength := aStream nextLittleEndianNumber: 2. "zip file comment"
	zipFileComment := aStream next: zipFileCommentLength.
