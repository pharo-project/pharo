accessing
atPath: anArray
	"Answer the element referenced by the give key path.
	Signal an error if not found."

	^self atPath: anArray ifAbsent: [self errorKeyNotFound: anArray]