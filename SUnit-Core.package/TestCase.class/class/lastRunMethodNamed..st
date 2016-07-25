history
lastRunMethodNamed: aSelector
	
	^ String streamContents: [:str |
		str nextPutAll: aSelector asString ;cr.
		str tab; nextPutAll: '^ ', (self lastRun) storeString]
