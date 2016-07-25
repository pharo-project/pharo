accessing
selectRowIndex: rowIndex event: event
	event shiftPressed ifTrue: [ ^ self selectRowIndexesUpTo: rowIndex ].
	event commandKeyPressed ifTrue: [ ^ self selectAppendingRowIndex: rowIndex ].
	self toggleRowIndex: rowIndex
