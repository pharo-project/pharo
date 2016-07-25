*CodeExport
fileOutOn: aFileStream initializing: aBool

	super fileOutOn: aFileStream.
	(aBool and:[ self includesSelector: #initialize ]) ifTrue: [
		aFileStream cr.
		aFileStream cr.
		aFileStream nextChunkPut: thisClass name , ' initialize'.
		aFileStream cr ]