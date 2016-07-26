column handling
addColumn: aTreeColumn afterIndex: aPosition
	"add a column at a given index then update the list
	in order to take the new column into account"
	aTreeColumn container: self.
	self columns add: aTreeColumn afterIndex: aPosition.
	self columnsChanged.
	self updateColumnMorphs.
	self updateList

