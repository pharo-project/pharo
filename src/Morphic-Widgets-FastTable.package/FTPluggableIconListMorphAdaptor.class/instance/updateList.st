updating
updateList
	(showIndex < self dataSource numberOfRows and: [ self isRowIndexVisible: showIndex ])
		ifFalse: [ self resetPosition ].
	self basicUpdateSelectionIndex.
	(self hasSelection and: [ (self isRowIndexVisible: self selectedRowIndex) not ])
		ifTrue: [ self resetPosition.
			self ensureVisibleFirstSelection ].
	self refresh