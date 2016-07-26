primitives
primWriteResult: fHandle
	"Answer the number of bytes written. A negative result means:
		-1 the last operation is still in progress
		-2 the last operation encountered an error"

	<primitive: 'primitiveAsyncFileWriteResult' module: 'AsynchFilePlugin'>
	self primitiveFailed
