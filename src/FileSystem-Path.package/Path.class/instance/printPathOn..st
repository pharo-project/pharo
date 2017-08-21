printing
printPathOn: aStream
	"Print the receiver's path on aStream (without 'Path' prepended) using the default delimiter"
	"String streamContents: [ :str| 
		((FileSystem workingDirectory / 'book-result' / 'W01-Welcome') 
			relativeToReference: FileSystem workingDirectory) printPathOn: str]
	>>> 'book-result/W01-Welcome'"

	self printPathOn: aStream delimiter: self delimiter.
