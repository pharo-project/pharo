writing
writeToFileNamed: aFileName prependingFileNamed: anotherFileName
	
	"Catch attempts to overwrite existing zip file"
	(self canWriteToFileNamed: aFileName)
		ifFalse: [ ^self error: (aFileName, ' is needed by one or more members in this archive') ].
	
	(File named: aFileName)
		writeStreamDo: [ :stream | self writeTo: stream prependingFileNamed: anotherFileName ]	