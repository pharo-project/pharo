reading
entryPositionsOn: aStream do: aBlock

	self flag: #todo. "Remove this method"
	self stream: aStream.
	^ self entryPositionsDo: aBlock.
