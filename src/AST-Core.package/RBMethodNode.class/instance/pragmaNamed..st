testing
pragmaNamed: aSymbol	
	^ self pragmaNamed: aSymbol ifAbsent: [ KeyNotFound signalFor: aSymbol  ]