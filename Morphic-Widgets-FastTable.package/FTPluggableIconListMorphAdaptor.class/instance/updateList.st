updating
updateList
	self resetPosition.
	self basicUpdateSelectionIndex.
	(self hasSelection and: [ (self isRowIndexVisible: self selectedRowIndex) not ])
		ifTrue: [ self ensureVisibleFirstSelection ].
	self refresh