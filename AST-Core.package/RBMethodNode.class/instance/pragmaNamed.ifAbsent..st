testing
pragmaNamed: aSymbol ifAbsent: absentBlock
	^ self pragmas 
		detect: [ :pragma| pragma selector = aSymbol ]
		ifNone: absentBlock