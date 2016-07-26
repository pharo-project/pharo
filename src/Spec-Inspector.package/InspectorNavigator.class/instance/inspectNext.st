actions
inspectNext
	history hasNext
		ifFalse: [ ^ self ].
	self inspectHistoryItem: history next