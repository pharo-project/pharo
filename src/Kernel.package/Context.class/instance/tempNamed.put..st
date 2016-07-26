accessing
tempNamed: aName put: anObject
	^self namedTempAt: (self tempNames indexOf: aName) put: anObject