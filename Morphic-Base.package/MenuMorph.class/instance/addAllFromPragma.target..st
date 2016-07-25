construction
addAllFromPragma: aString target: anObject
	self addAllFrom: (PragmaMenuBuilder 
		pragmaKeyword: aString
		model: anObject)
		menu 