accessing
commentAt: anEntry ifPresent: aBlock 

	^ commentByEntryReference
		at: (self referenceTo: anEntry)
		ifPresent: aBlock