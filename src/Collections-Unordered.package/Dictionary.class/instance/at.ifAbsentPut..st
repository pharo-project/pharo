accessing
at: key ifAbsentPut: aBlock
	"Return the value at the given key.
	If the key is not included in the receiver store and return the result 
	of evaluating aBlock as the new value."

	^ self at: key ifAbsent: [self at: key put: aBlock value]