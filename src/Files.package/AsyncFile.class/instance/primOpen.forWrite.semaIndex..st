primitives
primOpen: fileName forWrite: openForWrite semaIndex: semaIndex
	"Open a file of the given name, and return a handle for that file. Answer the receiver if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveAsyncFileOpen' module: 'AsynchFilePlugin'>
	^ nil
