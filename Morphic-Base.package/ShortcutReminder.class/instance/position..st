accessing
position: aSymbol

	(self positions includes: aSymbol) ifFalse: [ self error ].
	
	position := aSymbol