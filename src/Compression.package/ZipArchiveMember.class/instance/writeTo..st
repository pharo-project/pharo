writing
writeTo: aStream
	self rewindData.
	writeLocalHeaderRelativeOffset := aStream position.
	self writeLocalFileHeaderTo: aStream.
	self writeDataTo: aStream.
	self refreshLocalFileHeaderTo: aStream.