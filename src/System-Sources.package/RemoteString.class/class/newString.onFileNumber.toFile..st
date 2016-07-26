instance creation
newString: aString onFileNumber: sourceIndex toFile: aFileStream
	"Answer an instance of me for string, aString, on file indexed by 
	sourceIndex. Put the string on the file, aFileStream, and create the 
	remote reference. Assume that the index corresponds properly to 
	aFileStream."

	^self new string: aString onFileNumber: sourceIndex toFile: aFileStream