testing
includesCategory: aSymbol
	^ self item packages anySatisfy: [ :each | each name == aSymbol ]