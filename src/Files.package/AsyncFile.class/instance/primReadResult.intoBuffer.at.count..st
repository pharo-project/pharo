primitives
primReadResult: fHandle intoBuffer: buffer at: startIndex count: count
	"Copy the result of the last read operation into the given buffer starting at the given index. The buffer may be any sort of bytes or words object, excluding CompiledMethods. Answer the number of bytes read. A negative result means:
		-1 the last operation is still in progress
		-2 the last operation encountered an error"

	<primitive: 'primitiveAsyncFileReadResult' module: 'AsynchFilePlugin'>
	self primitiveFailed
