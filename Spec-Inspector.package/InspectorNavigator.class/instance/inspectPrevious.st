actions
inspectPrevious
	history hasPrevious
		ifFalse: [ ^ self ].
	self inspectHistoryItem: history previous