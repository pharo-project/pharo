private - primitives
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: name)
		signal: (self closed
			ifTrue: [ 'File ', name, ' is closed' ]
			ifFalse: [ 'File ', name, ' write failed' ])