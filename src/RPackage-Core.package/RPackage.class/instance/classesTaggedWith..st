simple symbol tags
classesTaggedWith: aSymbol
	"Any class could be tagged for user purpose.
	This method select all classes which are tagged by given symbol"
	
	^(self classTagNamed: aSymbol) classes