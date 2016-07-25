accessing
packageNamed: aSymbol
	^ self 
		packageNamed: aSymbol 
		ifAbsent: [ KeyNotFound signalFor: aSymbol ]