accessing selection
selectRowIndexes: anArray andMakeVisibleIf: shouldEnsureVisibleSelection
	| oldSelectedRowIndexes |
	anArray = selectedRowIndexes
		ifTrue: [ ^ self ].
	oldSelectedRowIndexes := selectedRowIndexes.
	self basicSelectRowIndexes: anArray.
	shouldEnsureVisibleSelection ifTrue: [ self ensureVisibleFirstSelection ].
	self refresh.
	self
		doAnnounce:
			((FTSelectionChanged from: oldSelectedRowIndexes to: selectedRowIndexes)
				fastTable: self;
				yourself)