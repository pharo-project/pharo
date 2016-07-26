instance creation
referencesToClassVariable: aSymbol in: aClass 
	^ self 
		on: self default
		referencesToClassVariable: aSymbol 
		in: aClass 