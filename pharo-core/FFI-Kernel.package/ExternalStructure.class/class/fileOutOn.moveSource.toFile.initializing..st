class management
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool
	super fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: aBool.
	(aBool and:[moveSource not]) ifTrue: 
		[aFileStream cr.
		aFileStream cr.
		aFileStream nextChunkPut: self name , ' compileFields'.
		aFileStream cr]