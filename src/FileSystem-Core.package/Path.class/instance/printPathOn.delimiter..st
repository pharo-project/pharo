printing
printPathOn: aStream delimiter: aCharacter
	"Print the path elements of the receiver, without the 'Path *' part"

	"String streamContents: [ :str| 
((FileSystem workingDirectory / 'book-result' / 'W01-Welcome') relativeToReference: FileSystem workingDirectory) printPathOn: str]
	>>> 'book-result/W01-Welcome'"
	
	(1 to: self size)
		do: [:index | aStream nextPutAll: (self at: index)]
		separatedBy: [aStream nextPut: aCharacter]