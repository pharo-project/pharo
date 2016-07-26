instance creation
existingNamed: aSymbol 
	^(self named: aSymbol)
		realName: aSymbol;
		yourself