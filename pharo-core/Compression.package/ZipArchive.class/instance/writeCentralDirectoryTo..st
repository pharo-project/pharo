private
writeCentralDirectoryTo: aStream
	| offset |
	offset := writeCentralDirectoryOffset.
	members do: [ :member |
		member writeCentralDirectoryFileHeaderTo: aStream.
		offset := offset + member centralDirectoryHeaderSize.
	].
	writeEOCDOffset := offset.
	self writeEndOfCentralDirectoryTo: aStream.

