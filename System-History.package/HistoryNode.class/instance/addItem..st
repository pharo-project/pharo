adding
addItem: anHistoryItem 
	self current notNil
		ifTrue: [(self current addItem: anHistoryItem)
				ifTrue: [^ true]].
	self opened
		ifTrue: [self history add: anHistoryItem.
			^ true].
	^ false