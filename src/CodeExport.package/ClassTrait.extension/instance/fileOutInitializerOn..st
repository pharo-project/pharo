*CodeExport
fileOutInitializerOn: aStream
	(self includesSelector: #initialize) ifTrue: 
		[aStream cr.
		aStream nextChunkPut: self soleInstance name , ' initialize'].