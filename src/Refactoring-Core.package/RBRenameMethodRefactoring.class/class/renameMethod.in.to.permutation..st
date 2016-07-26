instance creation
renameMethod: aSelector in: aClass to: newSelector permutation: aMap 
	^self new renameMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap