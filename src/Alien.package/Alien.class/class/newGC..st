instance creation
newGC: byteSize
	"Answer a new instance of the given byte size whose data resides on the external C heap,
	 and whose data will be reclaimed automatically when the instance is garbage collected.
	 As with instances residing on the Smalltalk heap the data is zero-filled."
	| alien address |
	(alien := self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated.
	address := self Ccalloc: byteSize.
	GCMallocedAliens add: alien finalizing: address.
	^alien
		addressFieldPut: address;
		initialize