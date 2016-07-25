conversions
writeQueryFields: dictionary withEncoder: encoder on: stream
	| first |
	first := true.
	dictionary keysAndValuesDo: [ :key :value | 
		first ifFalse: [ stream nextPut: $& ].
		first := false.
		stream nextPutAll: (self encodePercent: key safeSet: #queryKeyValueSafeSet encoder: encoder).
		value ifNotNil: [ 
			stream
				nextPut: $=;
				nextPutAll: (self encodePercent: value safeSet: #queryKeyValueSafeSet encoder: encoder) ] ]