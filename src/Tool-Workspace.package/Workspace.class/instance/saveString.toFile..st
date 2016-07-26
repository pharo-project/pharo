file support
saveString: aString toFile: aFileName
	"Save the given string to the given file"

	| writeStream fileContent fullFilename |
	fullFilename := aFileName asFileReference fullName.
	writeStream := (File named: fullFilename) openForWrite.
	fileContent := ZnCharacterWriteStream on: writeStream encoding: encoding.
	fileContent nextPutAll: aString