comparing
= other

	^ self class = other class and: [ 
		self rejectedEntryReferences = other rejectedEntryReferences ]