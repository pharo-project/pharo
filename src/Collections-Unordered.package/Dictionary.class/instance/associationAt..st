accessing
associationAt: key 
	"Returns the association for the given key."

	^ self associationAt: key ifAbsent: [self errorKeyNotFound: key]