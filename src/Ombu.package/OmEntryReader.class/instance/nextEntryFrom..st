reading
nextEntryFrom: aStream

	self flag: #todo. "Remove this method"
	self stream: aStream.
	^ self nextEntry