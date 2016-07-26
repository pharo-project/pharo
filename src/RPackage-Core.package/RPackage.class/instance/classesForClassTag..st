class tags
classesForClassTag: aSymbol
	"Returns the classes tagged using aSymbol"
	
	^ (self 
		classTagNamed: (self toTagName: aSymbol) 
		ifAbsent: [ ^ #() ])
		classes