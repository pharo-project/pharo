accessing
commentAt: anEntry ifAbsent: aBlock 

	^ commentByEntryReference
		at: (self referenceTo: anEntry)
		ifAbsent: aBlock