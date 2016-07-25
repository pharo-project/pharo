utilities
isMetaclassName: aSymbol
	"Validates if the arguments corresponds to a nonMetaClass"
	
	^(aSymbol
		indexOfSubCollection: ' class'
		startingAt: 1) > 0