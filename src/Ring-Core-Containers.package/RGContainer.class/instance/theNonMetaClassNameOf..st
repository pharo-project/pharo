utilities
theNonMetaClassNameOf: aSymbol
	"Rejects the prefix ' class' or ' classTrait' of the argument"
	| index |
	
	index := aSymbol
				indexOfSubCollection: ' class'
				startingAt: 1
				ifAbsent: [ ^aSymbol asSymbol ].

	^(aSymbol 
		copyFrom: 1
		to: index - 1) asSymbol