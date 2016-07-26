accessing
textSize: aSymbol

	(self sizes includes: aSymbol) ifFalse: [ self error ].
	
	size := aSymbol