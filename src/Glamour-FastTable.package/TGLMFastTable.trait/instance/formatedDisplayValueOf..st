accessing - cache
formatedDisplayValueOf: anElement
	^ self 
		cacheAt: #formatedDisplayValue 
		for: anElement 
		ifAbsentPut: [ self glamourPresentation formatedDisplayValueOf: anElement ]