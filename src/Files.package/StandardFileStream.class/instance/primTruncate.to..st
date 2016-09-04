private - primitives
primTruncate: id to: anInteger
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed
