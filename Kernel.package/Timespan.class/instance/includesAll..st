testing
includesAll: aCollection
	"Answer whether all the elements of aCollection are in the receiver."

	^ aCollection allSatisfy: [ :elem | self includes: elem ]