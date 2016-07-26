accessing
authorAt: anEntry

	^ self 
		authorAt: anEntry 
		ifAbsent: [ self error ]