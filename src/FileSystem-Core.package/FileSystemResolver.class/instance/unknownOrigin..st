resolving
unknownOrigin: aSymbol
	^ next ifNotNil: [next resolve: aSymbol]