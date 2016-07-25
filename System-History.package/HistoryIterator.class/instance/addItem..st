adding
addItem: anHistoryItem
	| result |
	self isPlugged ifFalse: [^ false].
	self recorder removeLast: (self size - self index).
	result := self recorder addItem: anHistoryItem.
	self updateIndex.
	^ result