accessing selection
selectRowIndexes: anArray
	| oldSelectedRowIndexes |
	anArray = selectedRowIndexes
		ifTrue: [ ^ self ].
	oldSelectedRowIndexes := selectedRowIndexes.
	self basicSelectRowIndexes: anArray.
	self ensureVisibleFirstSelection.
	self refresh.
	self
		doAnnounce:
			((FTSelectionChanged from: oldSelectedRowIndexes to: selectedRowIndexes)
				fastTable: self;
				yourself)