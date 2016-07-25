class tags
classTagNamed: aSymbol ifAbsent: aBlock 
	^ classTags 
		detect: [ :each | each name = aSymbol ]
		ifNone: aBlock