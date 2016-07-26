testing
isRowIndexVisible: rowIndex
	self exposedRows ifNil: [ ^ false ].
	^ self exposedRows includesKey: rowIndex