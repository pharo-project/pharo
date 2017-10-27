writing
writeTo: stream prependingFileNamed: aFileName
	| prepended buffer |
	stream binary.
	prepended := File openForReadFileNamed: aFileName.
	prepended binary.
	buffer := ByteArray new: (prepended size min: 32768).
	[ prepended atEnd ] whileFalse: [ | bytesRead |
		bytesRead := prepended readInto: buffer startingAt: 1 count: buffer size.
		stream next: bytesRead putAll: buffer startingAt: 1
	].
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset := stream position.
	self writeCentralDirectoryTo: stream.
	