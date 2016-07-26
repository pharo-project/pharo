encoding
encodeString: aString
	| length |
	(length := aString size) < 192
		ifTrue: [stream nextPut: length]
		ifFalse: 
			[stream nextPut: (length digitAt: 4)+192.
			stream nextPut: (length digitAt: 3).
			stream nextPut: (length digitAt: 2).
			stream nextPut: (length digitAt: 1)].
	stream nextBytesPutAll: aString.