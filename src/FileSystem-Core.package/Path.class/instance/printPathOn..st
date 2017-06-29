printing
printPathOn: aStream 
	"Print the path elements of the receiver, without the 'Path *' part"

	"String streamContents: [ :str| 
((FileSystem workingDirectory / 'book-result' / 'W01-Welcome') relativeToReference: FileSystem workingDirectory) printPathOn: str]
	>>> 'book-result/W01-Welcome'"
	
	self printPathOn: aStream delimiter: self delimiter