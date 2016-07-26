accessing selection
selectAllRowIndexes
	self isMultipleSelection ifFalse: [ ^ self ].
	self selectRowIndexes: (1 to: self numberOfRows) asArray 