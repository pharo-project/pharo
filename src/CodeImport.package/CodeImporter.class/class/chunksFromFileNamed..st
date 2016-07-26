instance creation
chunksFromFileNamed: aFileName
	^ (self fileNamed: aFileName)
			parseChunks;
			codeDeclarations.
