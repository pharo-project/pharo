visiting
visitReference: anEntry
	(block value: anEntry) 
		ifTrue: [ out nextPut: anEntry ]