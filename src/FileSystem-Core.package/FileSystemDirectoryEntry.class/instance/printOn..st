printing
printOn: aStream

	aStream nextPutAll: 'DirectoryEntry: '.
	reference ifNotNil: [:ref | aStream nextPutAll: reference printString].