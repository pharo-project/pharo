printing
printPathOn: aStream delimiter: aCharacter
	"Print the receiver's path on aStream (without 'Path' prepended)"
	"String streamContents: [ :str| 
		((FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
			relativeToReference: FileSystem workingDirectory) printPathOn: str delimiter: $|]
	>>> 'book-result|W01-Welcome'"

	(1 to: self size)
		do: [:index | aStream nextPutAll: (self at: index)]
		separatedBy: [aStream nextPut: aCharacter]