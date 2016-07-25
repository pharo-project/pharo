accessing
commitAt: anEntry ifPresent: aBlock 

	^ commitByEntryReference
		at: (self referenceTo: anEntry)
		ifPresent: aBlock