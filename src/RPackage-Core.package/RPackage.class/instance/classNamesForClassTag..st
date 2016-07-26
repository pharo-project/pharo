class tags
classNamesForClassTag: aSymbol
	"Returns the classes tagged using aSymbol"
	
	^ (self 
		classTagNamed: aSymbol 
		ifAbsent: [ ^ #() ]) classNames.
