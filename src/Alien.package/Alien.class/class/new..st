instance creation
new: byteSize
	"Answer a new instance of the given byte size whose data resides on the Smalltalk heap directly in the instance."
	^(self basicNew: byteSize + WordSize)
		sizeFieldPut: byteSize;
		initialize