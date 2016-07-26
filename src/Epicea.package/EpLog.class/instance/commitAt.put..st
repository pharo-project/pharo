accessing
commitAt: anEntry put: aValue 

	^ commitByEntryReference
		at: (self referenceTo: anEntry)
		put: aValue