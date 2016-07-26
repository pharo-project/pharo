primitives
primMalloc: aByteArray
	"copy aByteArray into newly allocated, external memory, and store the
	address of that memory in the receiver's handle"
	<primitive: 'primitiveMallocMemoryFaceData' module: 'FT2Plugin'>
	^self primitiveFailed