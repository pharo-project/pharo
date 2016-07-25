column handling
removeColumn: aTreeColumn
	"Remove a column - rough implementation"
	self removeColumnAtIndex: (self columns indexOf: aTreeColumn)