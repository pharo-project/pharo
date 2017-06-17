printing
printPathOn: aStream
	parent 
		ifNil: [ 
			aStream nextPutAll: initialTree repository workingDirectoryName ]
		ifNotNil: [ 	
			parent printPathOn: aStream.
			aStream nextPut: $/;
				nextPutAll: entryName ] 