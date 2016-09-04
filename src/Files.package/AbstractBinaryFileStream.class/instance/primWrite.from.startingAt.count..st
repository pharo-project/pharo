private
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: self name)
		signal: (self closed
			ifTrue: [ 'File ', self name, ' is closed' ]
			ifFalse: [ 'File ', self name, ' write failed' ])