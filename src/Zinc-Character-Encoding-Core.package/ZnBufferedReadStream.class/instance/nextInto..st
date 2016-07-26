accessing
nextInto: collection
	"Read the next elements of the receiver into collection,
	returning a copy if less elements are available"
	
	^ self
		next: collection size
		into: collection