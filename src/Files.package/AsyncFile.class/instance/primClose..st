primitives
primClose: fHandle
	"Close this file. Do nothing if primitive fails."

	<primitive: 'primitiveAsyncFileClose' module: 'AsynchFilePlugin'>
