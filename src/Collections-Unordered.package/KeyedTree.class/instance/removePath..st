removing
removePath: anArray
	"Remove and answer the element referenced by the given path.
	Signal an error if not found."

	^self removePath: anArray ifAbsent: [self errorKeyNotFound: anArray]