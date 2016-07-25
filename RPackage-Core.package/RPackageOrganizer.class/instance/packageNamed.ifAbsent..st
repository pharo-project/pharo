accessing
packageNamed: aSymbol  ifAbsent: errorBlock
	^ packages 
		at: aSymbol asSymbol 
		ifAbsent: errorBlock
	
	