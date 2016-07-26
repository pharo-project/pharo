stream in
next: requestedCount 
	"Read requestedCount bytes and return them as a ByteArray.
	If less are available, a smaller ByteArray will be returned."

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)