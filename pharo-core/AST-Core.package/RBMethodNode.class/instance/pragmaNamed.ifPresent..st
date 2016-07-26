testing
pragmaNamed: aSymbol ifPresent: foundBlock
	^ self pragmas 
		detect: [ :pragma| pragma selector = aSymbol ]
		ifFound: foundBlock