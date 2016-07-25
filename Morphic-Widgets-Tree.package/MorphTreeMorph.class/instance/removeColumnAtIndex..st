column handling
removeColumnAtIndex: aPosition.
	"remove a column at a given position - rough implementation"
	self columns removeAt: aPosition.
	self columnsChanged.
	self updateList.




