accessing
next: requestedCount into: collection
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self 
		next: requestedCount 
		into: collection 
		startingAt: 1   