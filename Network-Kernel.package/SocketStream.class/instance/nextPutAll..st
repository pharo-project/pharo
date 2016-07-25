stream out
nextPutAll: aCollection
	"Put a String or a ByteArray onto the stream.
	Currently a large collection will allocate a large buffer."

	| toPut |
	toPut := binary ifTrue: [aCollection asByteArray] ifFalse: [aCollection asString].
	self adjustOutBuffer: toPut size.
	outBuffer replaceFrom: outNextToWrite to: outNextToWrite + toPut size - 1 with: toPut startingAt: 1.
	outNextToWrite := outNextToWrite + toPut size.
	self checkFlush.
	^aCollection