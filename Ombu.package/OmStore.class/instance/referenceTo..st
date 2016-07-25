accessing
referenceTo: anEntry
	"Answer a reference to an entry."
	
	^ anEntry tags 
		at: self selfReferenceKey 
		ifAbsent: [ self error: 'Invalid entry: ', anEntry asString ]