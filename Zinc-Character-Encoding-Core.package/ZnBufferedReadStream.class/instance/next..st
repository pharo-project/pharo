accessing
next: requestedCount 
	"Read requestedCount elements and return them as a collection.
	If less are available, a smaller collection will be returned."

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)