accessing
authorAt: anEntry ifAbsent: aBlock

	^ anEntry tags 
		at: self class authorKey
		ifAbsent: aBlock