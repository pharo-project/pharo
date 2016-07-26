testing
includesAny: aCollection
	"Answer whether any element of aCollection is included in the receiver"

	^ aCollection anySatisfy: [ :elem | self includes: elem ]