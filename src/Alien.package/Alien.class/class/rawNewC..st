instance creation
rawNewC: byteSize
	"Answer a new instance of the given byte size whose data resides on the external C heap.
	 Unlike newC: the data is *not* zero-filled."
	^(self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated;
		addressFieldPut: (self Cmalloc: byteSize);
		initialize