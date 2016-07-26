private
ensureVisibleFirstSelection
	| rowIndex |
	self hasSelection ifFalse: [ ^ self ].
	rowIndex := self selectedRowIndex.
	(self container isRowIndexFullyVisible: rowIndex) ifFalse: [ 
		rowIndex < self showIndex 
			ifTrue: [ self moveShowIndexTo: rowIndex ]
			ifFalse: [ self moveShowIndexTo: (rowIndex - self container calculateMinVisibleRows + 1) ] ]