accessing
associationAt: key 
	"Returns an association for the given key.

	Modifying the association won't affect the receiver because it
	isn't implemented with associations like Dictionary."

	^ self associationAt: key ifAbsent: [self errorKeyNotFound: key]