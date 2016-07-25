serializing
serializeInBinary: aSnapshot
	| writer s |
	s := RWBinaryOrTextStream on: String new.
	writer := MCDataStream on: s.
	writer nextPut: aSnapshot.
	^ s contents