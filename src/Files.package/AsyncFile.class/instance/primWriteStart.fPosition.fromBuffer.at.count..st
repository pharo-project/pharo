primitives
primWriteStart: fHandle fPosition: fPosition fromBuffer: buffer at: startIndex count: count
	"Start a write operation of count bytes starting at the given index in the given buffer. The buffer may be any sort of bytes or words object, excluding CompiledMethods. The contents of the buffer are copied into an internal buffer immediately, so the buffer can be reused after the write operation has been started. Fail if there is insufficient C heap to allocate an internal buffer of the requested size."

	<primitive: 'primitiveAsyncFileWriteStart' module: 'AsynchFilePlugin'>
	writeable ifFalse: [^ self error: 'attempt to write a file opened read-only'].
	self primitiveFailed
