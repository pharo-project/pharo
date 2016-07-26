writing
writeTo: stream prepending: aString
	stream binary.
	stream nextPutAll: aString.
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset := stream position.
	self writeCentralDirectoryTo: stream.
	