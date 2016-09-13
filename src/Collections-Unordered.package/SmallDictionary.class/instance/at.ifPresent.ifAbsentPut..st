accessing
at: key ifPresent: aPresentBlock ifAbsentPut: anAbsentBlock
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the first block optionally with the value associated
	with the key.
	Otherwise store and return the result of evaluating the second block as the
	new value of the key."

	^ self
		at: key
		ifPresent: aPresentBlock
		ifAbsent: [ self at: key put: anAbsentBlock value ]