updating
updateList
	(showIndex < self dataSource numberOfRows and: [ self isRowIndexVisible: showIndex ])
		ifFalse: [ self resetPosition ].
	self basicUpdateSelectionIndex.
	(self hasSelection and: [ (self isRowIndexVisible: self selectedRowIndex) ])
		ifFalse: [ self resetPosition.
			self ensureVisibleFirstSelection ].
	self refresh