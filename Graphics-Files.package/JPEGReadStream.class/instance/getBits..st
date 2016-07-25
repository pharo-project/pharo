accessing
getBits: requestedBits 
	| value |
	requestedBits > bitsInBuffer ifTrue: 
		[ self fillBuffer.
		requestedBits > bitsInBuffer ifTrue: [ self error: 'not enough bits available to decode' ] ].
	value := bitBuffer bitShift: requestedBits - bitsInBuffer.
	bitBuffer := bitBuffer bitAnd: (1 bitShift: bitsInBuffer - requestedBits) - 1.
	bitsInBuffer := bitsInBuffer - requestedBits.
	^ value