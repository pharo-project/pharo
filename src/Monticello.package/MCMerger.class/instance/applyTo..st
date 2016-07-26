operations
applyTo: anObject
	self isMerged ifFalse: [self error: 'You must resolve all the conflicts first'].
	self operations do: [:ea | ea applyTo: anObject]