layout-properties
reverseTableCells: aBool
	"Layout specific. This property describes if the cells should be treated in reverse order of submorphs."
	self assureTableProperties reverseTableCells: aBool.
	self layoutChanged.