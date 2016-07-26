convenience
withPathSegment: segment
	"Return a new URL equal to the receiver with its path extended with segment"
	
	^ self copy
		addPathSegment: segment;
		yourself