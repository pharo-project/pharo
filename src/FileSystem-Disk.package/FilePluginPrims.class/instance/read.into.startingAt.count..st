file primitives
read: id into: byteArray startingAt: startIndex count: count
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self primitiveFailed