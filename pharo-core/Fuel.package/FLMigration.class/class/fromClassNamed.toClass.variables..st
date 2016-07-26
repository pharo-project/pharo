instance creation
fromClassNamed: aSymbol toClass: aClass variables: anArray 

	^self basicNew
		initializeClassNamed: aSymbol 
		toClass: aClass 
		variables: anArray;
		yourself.