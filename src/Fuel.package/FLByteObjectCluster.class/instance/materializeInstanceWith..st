serialize/materialize
materializeInstanceWith: aDecoder

	| instance size |
	size := aDecoder nextEncodedPositiveInteger.
	instance := theClass basicNew: size.
	aDecoder nextEncodedBytesInto: instance.
	^ instance
