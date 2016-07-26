operation
removeBefore: anIndex
	anIndex = 1 ifTrue: [
		"remove ourselves"
		self delete.
		].
	