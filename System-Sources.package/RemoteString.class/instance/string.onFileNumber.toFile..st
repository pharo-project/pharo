chunk
string: aString onFileNumber: fileNumber toFile: aFileStream 
	"Store this as the receiver's string if source files exist."

	| position |
	position := aFileStream position.
	self fileNumber: fileNumber position: position.
	aFileStream nextChunkPut: aString.
	^ self 