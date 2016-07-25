layout-properties
minCellSize: aPoint
	"Layout specific. This property specifies the minimal size of a table cell."
	self assureTableProperties minCellSize: aPoint.
	self layoutChanged.