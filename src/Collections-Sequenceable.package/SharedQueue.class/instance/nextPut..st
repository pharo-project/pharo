accessing
nextPut: anObject
	monitor critical: [
		items addLast: anObject.
		monitor signal].
	^ anObject
