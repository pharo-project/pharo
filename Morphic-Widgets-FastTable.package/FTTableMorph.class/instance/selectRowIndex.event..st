private
selectRowIndex: rowIndex event: event
	rowIndex
		ifNotNil: [ selectionStrategy selectRowIndex: rowIndex event: event ]
		ifNil: [ self selectRowIndexes: #() ].