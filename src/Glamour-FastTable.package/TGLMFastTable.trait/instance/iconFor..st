accessing - cache
iconFor: anElement
	^ self 
		cacheAt: #icon 
		for: anElement 
		ifAbsentPut: [ self glamourPresentation icon cull: anElement ]