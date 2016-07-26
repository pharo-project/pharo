accessing
timeAt: anEntry ifAbsent: aBlock

	^ anEntry tags 
		at: self class timeKey
		ifAbsent: aBlock