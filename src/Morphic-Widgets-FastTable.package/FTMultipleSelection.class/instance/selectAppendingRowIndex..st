private
selectAppendingRowIndex: rowIndex
	| currentSelection newSelection |
	currentSelection := self table selectedRowIndexes.
	
	newSelection := (currentSelection includes: rowIndex)
		ifTrue: [ currentSelection copyWithout: rowIndex ]
		ifFalse: [ currentSelection copyWithFirst: rowIndex].
		
	self table selectRowIndexes: newSelection