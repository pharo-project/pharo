instance creation
newC: byteSize
	"Answer a new instance of the given byte size whose data resides on the external C heap.
	 As with instances residing on the Smalltalk heap the data is zero-filled."
	^(self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated;
		addressFieldPut: (self Ccalloc: byteSize);
		initialize