private-reading
skipLocalDirectoryFileHeaderFrom: aStream 
	"Assumes that stream is positioned after signature."

	|  extraFieldLength fileNameLength |
	aStream next: 22.
	fileNameLength := aStream nextLittleEndianNumber: 2.
	extraFieldLength := aStream nextLittleEndianNumber: 2.
	aStream next: fileNameLength.
	aStream next: extraFieldLength.
	dataOffset := aStream position.
