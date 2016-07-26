private
toggleRowIndex: rowIndex
	((self table selectedRowIndexes includes: rowIndex) 
		and: [ self table selectedRowIndexes size = 1 "just one selected"
		and: [ self table allowsDeselection ] ])
			ifTrue: [  self table selectRowIndexes: #() ]
			ifFalse: [
				self table selectRowIndex: rowIndex.
				self table highlightRowIndexes: #() ]