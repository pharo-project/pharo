adding
add: newObject withOccurrences: anInteger 
	"Add newObject anInteger times to the receiver. Answer newObject."

	contents at: newObject put: (contents at: newObject ifAbsent: [0]) + anInteger.
	^ newObject