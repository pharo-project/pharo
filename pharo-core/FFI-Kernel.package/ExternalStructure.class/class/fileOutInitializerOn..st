class management
fileOutInitializerOn: aFileStream
	super fileOutInitializerOn: aFileStream.
	aFileStream cr.
	aFileStream cr.
	aFileStream nextChunkPut: self name , ' compileFields'.
	aFileStream cr.