access
fileName
	^(file ifNotNil: [ file name ]) 
			ifNil: [ '<no file>' ]