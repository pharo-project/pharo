accessing selection
selectedRowIndex
	^ self selectedRowIndexes 
		ifNotEmpty: [ :indexes | indexes first ]
		ifEmpty: [ 0 ] 