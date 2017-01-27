file primitives
sizeOf: id
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed
