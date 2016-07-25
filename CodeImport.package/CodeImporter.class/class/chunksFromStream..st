instance creation
chunksFromStream: aStream
	^ (self fileStream: aStream)
			parseChunks;
			codeDeclarations.
