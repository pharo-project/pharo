convenience
withPathSegments: pathSegments
	"Return a new URL equal to the receiver with its path extended with pathSegments"
	
	^ self copy
		addPathSegments: pathSegments;
		yourself