accessing
fontList
	
	^ fontList ifNil: [ self refreshFontList ].